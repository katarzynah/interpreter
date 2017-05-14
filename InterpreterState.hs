module InterpreterState where

import Control.Monad.State 

import AbsInterpreter
import Value
import ProgramState

runProgram :: Program -> IO ()
runProgram program = do
  (_, env) <- runStateT (transProgram program) emptyGEnv
  print env
  return ()

transProgram :: Program -> ProgramState ()
transProgram x = case x of
  Prog _ declarations compoundStmt -> do
    transDeclarations declarations
    transCompoundStatement compoundStmt

transDeclarations :: Declarations -> ProgramState ()
transDeclarations x = case x of
  Dec varDeclarations procDeclarations -> do
    transVariableDeclarations varDeclarations
    transProcedureDeclarations procDeclarations

transVariableDeclarations :: VariableDeclarations -> ProgramState ()
transVariableDeclarations x = case x of
  VarDecEmpty -> return ()
  VarDecFull varDeclarationList ->
    transVariableDeclarationList varDeclarationList

transVariableDeclarationList :: VariableDeclarationList -> ProgramState ()
transVariableDeclarationList x = case x of
  VarDecListEnd varDecl -> transVarDec varDecl
  VarDecList varDecl varDeclarationlist -> do
    transVarDec varDecl
    transVariableDeclarationList varDeclarationlist

transVarDec :: VarDec -> ProgramState ()
transVarDec x = case x of
  VarDecLabel idList typeSpecifier -> do
    let dimensions = getDimsFromTypeSpec typeSpecifier
    declareVars (getIdentsFromIdList idList) dimensions

transProcedureDeclarations :: ProcedureDeclarations -> ProgramState ()
transProcedureDeclarations x = case x of
  ProcDecEmpty -> return ()
  ProcDecLabel procDecl procDeclarations -> do
    transProcDec procDecl
    transProcedureDeclarations procDeclarations

transProcDec :: ProcDec -> ProgramState ()
transProcDec x = do
  setDecl (getProcDecIdent x) x

transCompoundStatement :: CompoundStatement -> ProgramState ()
transCompoundStatement x = case x of
  CompStmnt statementList -> transStatementList statementList

transStatementList :: StatementList -> ProgramState ()
transStatementList x = case x of
  StmntListEmpty -> return ()
  StmntList statement statementList -> do
    transStatement statement
    transStatementList statementList

transStatement :: Statement -> ProgramState ()
transStatement x = case x of
  SEmpty -> return ()
  SComp compoundStmnt -> transCompoundStatement compoundStmnt
  SAss assignmentStmnt -> transAssignmentStatement assignmentStmnt
  SProc procCall -> transProcedureCall procCall
  SFor forStmnt -> transForStatement forStmnt
  SWhile whileStmnt -> transWhileStatement whileStmnt
  SIf ifStmnt -> transIfStatement ifStmnt
  SPrint printStmnt -> transPrintStatement printStmnt

transAssignmentStatement :: AssignmentStatement -> ProgramState ()
transAssignmentStatement x = case x of
  AssStmnt ident expression -> do
    val <- transExpression expression
    setVarVal ident val
  AssStmntArr ident expressionList expression -> do
    val <- transExpression expression
    let expressions = transExpressionList expressionList
    values <- evaluateArguments expressions
    let dims = evaluateToInts values
    setArrayVal ident dims val

-- Given identificators of procedure arguments, the values of expressions with
-- which the procedure was called and declarations in the procedure, updates
-- the environment in the procedure.
setUpProcEnv :: [Ident] -> [Value] -> Declarations -> ProgramState ()
setUpProcEnv idents values declarations = do
  if (length values) /= (length idents) then error "Wrong number of arguments."
  else do
    -- adding a new local environment to our environments
    addLocalEnvironment
    -- declaring procedure arguments in the environment
    declareVars idents []
    -- setting the values of arguments in the environment
    setVarsVals idents values
    -- adding local variable declarations
    transDeclarations declarations

transProcedureCall :: ProcedureCall -> ProgramState ()
transProcedureCall x = case x of
  ProcCall ident actuals -> do
    procDec <- getDecl ident --here
    let arguments = transActuals actuals
    values <- evaluateArguments arguments
    case procDec of
      (ProcDecProc procHeader declarations compoundStmnt) -> do
        let argumentIdents = getIdentsFromProcHeader procHeader
        setUpProcEnv argumentIdents values declarations
        transCompoundStatement compoundStmnt
        exitLocalEnvironment

transFunctionCall :: FunctionCall -> ProgramState Value
transFunctionCall x = case x of
  FunsCall ident actuals -> do
    funcDec <- getDecl ident
    let arguments = transActuals actuals
    values <- evaluateArguments arguments
    case funcDec of
      (ProcDecFun funcHeader declarations compoundStmnt) -> do
        let argumentIdents = getIdentsFromFuncHeader funcHeader
        let returnValueDim = getDimsFromFuncHeader funcHeader
        setUpProcEnv argumentIdents values declarations
        -- adding variable with same identificator as function to the environment
        declareVar ident returnValueDim
        transCompoundStatement compoundStmnt
        val <- getVarVal ident
        exitLocalEnvironment
        return (val)
      _ -> error "Procedures don't return values."

-- Executes the for loop, ident is the identyficator of the counter variable,
-- endValue is the value which causes the loop to finish when counter reaches
-- it.
executeForStatement :: Ident -> Value -> Statement -> ProgramState ()
executeForStatement ident endValue statement = do
  i <- getVarVal ident
  case (i, endValue) of
    (VInt x, VInt y) ->
      if i > endValue then return ()
      else do
        transStatement statement
        setVarVal ident (VInt(x + 1))
        executeForStatement ident endValue statement
    _ -> error ("For statement can only be used with expressions that " ++
                "evaluate to integers.")

transForStatement :: ForStatement -> ProgramState ()
transForStatement x = case x of
  ForStmnt ident expression1 expression2 statement -> do
    val1 <- transExpression expression1
    val2 <- transExpression expression2
    setVarVal ident val1
    executeForStatement ident val2 statement

transWhileStatement :: WhileStatement -> ProgramState ()
transWhileStatement x = case x of
  WhileStmnt expression statement -> do
    val <- transExpression expression
    case val of
      VBool(True) -> do
        transStatement statement
        transWhileStatement x
      VBool(False) -> return ()
      _ -> error ("While statement can only be used with expressions that " ++
                  "evaluate to boolean values.")

transIfStatement :: IfStatement -> ProgramState ()
transIfStatement x = case x of
  IfStmnt expression statement -> do
    val <- transExpression expression
    case val of
      VBool(True) -> transStatement statement
      VBool(False) -> return ()
      _ -> error ("If statement can only be used with expressions that " ++
                  "evaluate to boolean values.")
  IfStmntWithElse expression statement1 statement2 -> do
    val <- transExpression expression
    case val of
      VBool(True) -> transStatement statement1
      VBool(False) -> transStatement statement2
      _ -> error ("If statement can only be used with expressions that " ++
                  "evaluate to boolean values.")

_evaluateArguments :: [Expression] -> [Value] -> ProgramState [Value]
_evaluateArguments [] values = return (values)
_evaluateArguments (expr : exprs) values = do
  val <- transExpression expr
  _evaluateArguments exprs (values ++ [val])

-- Given a list of expressions returns list of their values.
evaluateArguments :: [Expression] -> ProgramState [Value]
evaluateArguments exprs  = _evaluateArguments exprs []

executePrint :: [Value] -> IO ()
executePrint [] = do
  putStrLn ""
  return ()
executePrint (x : []) = do
  putStrLn (show x)
  return ()
executePrint (x : xs) = do
  putStr (show x)
  putStr " "
  executePrint xs

transPrintStatement :: PrintStatement -> ProgramState ()
transPrintStatement x = case x of
  PrintStmnt actuals -> do
    let expressions = transActuals actuals
    values <- evaluateArguments expressions
    liftIO $ executePrint values

-- Given two simple expressions and a comparison function, evaluates them and
-- returns the result of the function applied to the values.
compareExpressions :: (SimpleExpression -> SimpleExpression -> 
                       (Value -> Value -> Bool) -> ProgramState Value)
compareExpressions simExp1 simExp2 comparer = do
  val1 <- transSimpleExpression simExp1
  val2 <- transSimpleExpression simExp2
  return (VBool(comparer val1 val2))

transExpression :: Expression -> ProgramState Value
transExpression x = case x of
  ExpSimple simpleExpr -> transSimpleExpression simpleExpr
  ExpEqual simpleExpr1 simpleExpr2 ->
    (compareExpressions simpleExpr1 simpleExpr2 (==))
  ExpNotEqual simpleExpr1 simpleExpr2 ->
    (compareExpressions simpleExpr1 simpleExpr2 (/=))
  ExpLess simpleExpr1 simpleExpr2 ->
    (compareExpressions simpleExpr1 simpleExpr2 (<))
  ExpLessOrEqual simpleExpr1 simpleExpr2 ->
    (compareExpressions simpleExpr1 simpleExpr2 (<=))
  ExpGreater simpleExpr1 simpleExpr2 ->
    (compareExpressions simpleExpr1 simpleExpr2 (>))
  ExpGreaterOrEqual simpleExpr1 simpleExpr2 ->
    (compareExpressions simpleExpr1 simpleExpr2 (>=))

-- Given simple expression, term and a binary function on ints, evaluates them
-- and returns the result of the function. 
computeSimpleExpression :: (SimpleExpression -> Term -> (Int -> Int -> Int) ->
                            ProgramState Value)
computeSimpleExpression simpleExpr term fun = do
  val1 <- transSimpleExpression simpleExpr
  val2 <- transTerm term
  case (val1, val2) of
      (VInt x, VInt y) -> return (VInt(fun x y))
      _ -> error "Can only add/substract integers."

transSimpleExpression :: SimpleExpression -> ProgramState Value
transSimpleExpression x = case x of
  SimpleExpTerm term -> transTerm term
  SimpleExpAdd simpleExpr term -> computeSimpleExpression simpleExpr term (+)
  SimpleExpSubst simpleExpr term -> computeSimpleExpression simpleExpr term (-)

transTerm :: Term -> ProgramState Value
transTerm x = case x of
  TermFactor factor -> transFactor factor
  TermMultiply term factor -> do
    val1 <- transTerm term
    val2 <- transFactor factor
    case (val1, val2) of
        (VInt x, VInt y) -> return (VInt(x * y))
        _ -> error "Can only multipy integers."
  TermDivide term factor -> do
    val1 <- transTerm term
    val2 <- transFactor factor
    case (val1, val2) of
        (VInt x, VInt y) -> if y /=0 then return (VInt (div x y))
                            else error "Can't divide by 0."
        _ -> error "Can only divide integers."

-- Given factor, int and environment, evaluates the factor and returns its value
-- multiplied by the integer in context.
computeTransFactor :: Factor -> Int -> ProgramState Value
computeTransFactor factor int = do
  val <- transFactor factor
  case val of
      VInt x -> return (VInt (int * x))
      _ -> error "Can only add '+'/'-' before integers."

transFactor :: Factor -> ProgramState Value
transFactor x = case x of
  FactorExpression expression -> transExpression expression
  FactorPlus factor -> computeTransFactor factor 1
  FactorMinus factor -> computeTransFactor factor (-1)
  FactorFunctionCall functionCall -> transFunctionCall functionCall
  FactorConstant constant -> return (transConstant constant)
  FactorIdent ident -> getVarVal ident
  FactorArray ident expressionList -> do
    let expressions = transExpressionList expressionList
    values <- evaluateArguments expressions
    let dims = evaluateToInts values
    getArrayVal ident dims
  FactorStoI expression -> do
    val <- transExpression expression
    case val of
      (VStr str) -> return (VInt (read str :: Int))
      _ -> error "Can only use 'string_to_int' on strings."
  FactorItoS expression -> do
    val <- transExpression expression
    case val of
      (VInt val) -> return (VStr (show val))
      _ -> error "Can only use 'int_to_string' on integers."

transActuals :: Actuals -> [Expression]
transActuals x = case x of
  ActEmpty -> []
  Act expressionList -> transExpressionList expressionList

transExpressionList :: ExpressionList -> [Expression]
transExpressionList x = case x of
  ExpListOne expression -> [expression]
  ExpList expression expressionList ->
    [expression] ++ transExpressionList expressionList

transConstant :: Constant -> Value
transConstant x = case x of
  ConstInt integer -> VInt (fromInteger integer)
  ConstBool boolean -> transBoolean boolean
  ConstString string -> VStr string

transBoolean :: Boolean -> Value
transBoolean x = case x of
  BoolTrue -> VBool True
  BoolFalse -> VBool False

-- Functions for getting identifiers and dimension lists.

-- Gets procedure/function identifier from procedure/function declaration.
getProcDecIdent :: ProcDec -> Ident
getProcDecIdent (ProcDecProc (ProcHead id _) _ _) = id
getProcDecIdent (ProcDecFun (FunHead id _ _) _ _) = id

-- Gets identifiers of arguments from procedure header.
getIdentsFromProcHeader :: ProcHeader -> [Ident]
getIdentsFromProcHeader x = case x of
  ProcHead ident arguments -> getIdentsFromArguments arguments

-- Gets identifiers of arguments from function header.
getIdentsFromFuncHeader :: FuncHeader -> [Ident]
getIdentsFromFuncHeader x = case x of
  FunHead ident arguments typeSpecifier -> getIdentsFromArguments arguments

getIdentsFromArguments :: Arguments -> [Ident]
getIdentsFromArguments x = case x of
  ArgsEmpty -> []
  Args argumentList -> getIdentsFromArgList argumentList

getIdentsFromArgList :: ArgumentList -> [Ident]
getIdentsFromArgList x = case x of
  ArgListOne arg -> getIdentsFromArg arg
  ArgList arg argumentList ->
    getIdentsFromArg arg ++ getIdentsFromArgList argumentList

getIdentsFromArg :: Arg -> [Ident]
getIdentsFromArg x = case x of
  ArgLabel idList typeSpecifier -> getIdentsFromIdList idList

getIdentsFromIdList :: IdList -> [Ident]
getIdentsFromIdList x = case x of
  IdLEnd ident -> [ident]
  IdL ident idList -> [ident] ++ getIdentsFromIdList idList

getDimsFromFuncHeader :: FuncHeader -> [Int]
getDimsFromFuncHeader x = case x of
  FunHead ident arguments typeSpecifier -> getDimsFromTypeSpec typeSpecifier

getDimsFromTypeSpec :: TypeSpecifier -> [Int]
getDimsFromTypeSpec x = case x of
  TypeSpecInt -> []
  TypeSpecBool -> []
  TypeSpecString -> []
  TypeSpecArray dimensionList typeSpecifier ->
    getDimsFromDimsList dimensionList ++ getDimsFromTypeSpec typeSpecifier

getDimsFromDimsList :: DimensionList -> [Int]
getDimsFromDimsList x = case x of
  DimListEnd integer -> [(fromInteger integer)]
  DimList integer dimensionList ->
    ((fromInteger integer) : getDimsFromDimsList dimensionList)

