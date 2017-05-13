module Interpreter where

import AbsInterpreter
import Environment

transProgram :: Program -> IO()
transProgram x = case x of
  Prog _ declarations compoundStmt -> do
    env <- transDeclarations declarations emptyGEnv
    print env --TODO: used in tests!!! but remove before turning in
    _ <- transCompoundStatement compoundStmt env
    return ()

transDeclarations :: Declarations -> GEnv -> IO GEnv
transDeclarations x env = case x of
  Dec varDeclarations procDeclarations -> do
    env' <- transVariableDeclarations varDeclarations env
    env'' <- transProcedureDeclarations procDeclarations env'
    return env''

transVariableDeclarations :: VariableDeclarations -> GEnv -> IO GEnv
transVariableDeclarations x env = case x of
  VarDecEmpty -> return env
  VarDecFull varDeclarationList ->
    transVariableDeclarationList varDeclarationList env

transVariableDeclarationList :: VariableDeclarationList -> GEnv -> IO GEnv
transVariableDeclarationList x env = case x of
  VarDecListEnd varDecl -> transVarDec varDecl env
  VarDecList varDecl varDeclarationlist -> do
    env' <- transVarDec varDecl env
    env'' <- transVariableDeclarationList varDeclarationlist env'
    return env''

transVarDec :: VarDec -> GEnv -> IO GEnv
transVarDec x env = case x of
  VarDecLabel idList typeSpecifier -> do
    let dimensions = transTypeSpecifier typeSpecifier
    return (declareVars env (getIdentsFromIdList idList) dimensions)
    
transProcedureDeclarations :: ProcedureDeclarations -> GEnv -> IO GEnv
transProcedureDeclarations x env = case x of
  ProcDecEmpty -> return env
  ProcDecLabel procDecl procDeclarations -> do
    env' <- transProcDec procDecl env
    env'' <- transProcedureDeclarations procDeclarations env'
    return env''

-- Gets identifier from procedure/function declaration.
getProcDecIdent :: ProcDec -> Ident
getProcDecIdent (ProcDecProc (ProcHead id _) _ _) = id
getProcDecIdent (ProcDecFun (FunHead id _ _) _ _) = id

transProcDec :: ProcDec -> GEnv -> IO GEnv
transProcDec x env = return (setDecl env (getProcDecIdent x) x)

transCompoundStatement :: CompoundStatement -> GEnv -> IO GEnv
transCompoundStatement x env = case x of
  CompStmnt statementList -> transStatementList statementList env

transStatementList :: StatementList -> GEnv -> IO GEnv
transStatementList x env = case x of
  StmntListEmpty -> return env
  StmntList statement statementList -> do
    env' <- transStatement statement env
    env'' <- transStatementList statementList env'
    return env''

transStatement :: Statement -> GEnv -> IO GEnv
transStatement x env = case x of
  SEmpty -> return env
  SComp compoundStmnt -> transCompoundStatement compoundStmnt env
  SAss assignmentStmnt -> transAssignmentStatement assignmentStmnt env
  SProc procCall -> transProcedureCall procCall env
  SFor forStmnt -> transForStatement forStmnt env
  SWhile whileStmnt -> transWhileStatement whileStmnt env
  SIf ifStmnt -> transIfStatement ifStmnt env
  SPrint printStmnt -> transPrintStatement printStmnt env

transAssignmentStatement :: AssignmentStatement -> GEnv -> IO GEnv
transAssignmentStatement x env = case x of
  AssStmnt ident expression -> do
    (val, env') <- transExpression expression env
    return (setVarVal env' ident val)
  AssStmntArr ident expressionList expression -> do
    (val, env') <- transExpression expression env
    let expressions = transExpressionList expressionList
    (values, env'') <- evaluateArguments expressions env'
    let dims = evaluateToInts values
    return (setArrayVal env'' ident dims val)

_evaluateArguments :: [Expression] -> GEnv -> [Value] -> IO([Value], GEnv)
_evaluateArguments [] env values = do return (values, env)
_evaluateArguments (expr : exprs) env values = do
  (val, env') <- transExpression expr env
  _evaluateArguments exprs env' (values ++ [val])

-- Given a list of expressions evaluates them in given environment, and returns
-- list of their values and new environment in context.
evaluateArguments :: [Expression] -> GEnv -> IO([Value], GEnv)
evaluateArguments exprs env = _evaluateArguments exprs env []

-- Given identificators of procedure arguments, the values of expressions with
-- which the procedure was called and declarations in the procedure, generates
-- the environment in the procedure.
setUpProcEnv :: [Ident] -> [Value] -> Declarations -> GEnv -> IO GEnv
setUpProcEnv idents values declarations env = do
  if (length values) /= (length idents) then
    error ("Wrong number of arguments")
  else do
    -- adding a new local environment to our environments
    let newEnv = (emptyEnv : env)
    -- declaring procedure arguments in the environment
    let newEnvWithUndefinedArgs = declareVars newEnv idents []
    -- setting the values of arguments in the environment
    let newEnvWithArgs = setVarsVals newEnvWithUndefinedArgs idents values
    -- adding local variable declarations
    transDeclarations declarations newEnvWithArgs

transProcedureCall :: ProcedureCall -> GEnv -> IO GEnv
transProcedureCall x env = case x of
  ProcCall ident actuals -> do
    let procDec = getDecl env ident
    let arguments = transActuals actuals
    (values, env') <- evaluateArguments arguments env
    case procDec of
      (ProcDecProc procHeader declarations compoundStmnt) -> do
        let argumentIdents = getIdentsFromProcHeader procHeader
        env'' <- setUpProcEnv argumentIdents values declarations env'
        finalEnv <- transCompoundStatement compoundStmnt env''
        case finalEnv of
          (localEnv : env) -> return env

-- Given identificators of procedure arguments, the values of expressions with
-- which the procedure was called, the declarations inside it, the identificator
-- of the function and dimenstion list of its return type, generates the
-- environment in the function.
setUpFuncEnv :: ([Ident] -> [Value] -> Declarations -> Ident -> [Int] -> GEnv ->
                 IO GEnv)
setUpFuncEnv idents values declarations ident dims env = do
  env' <- setUpProcEnv idents values declarations env
  -- adding variable with same identificator as function to the environment
  return (declareVar env' ident dims)

transFunctionCall :: FunctionCall -> GEnv -> IO (Value, GEnv)
transFunctionCall x env = case x of
  FunsCall ident actuals -> do
    let funcDec = getDecl env ident
    let arguments = transActuals actuals
    (values, env') <- evaluateArguments arguments env
    case funcDec of
      (ProcDecFun funcHeader declarations compoundStmnt) -> do
        let argumentIdents = getIdentsFromFuncHeader funcHeader
        let returnValueDim = getDimsFromFuncHeader funcHeader
        env'' <- setUpFuncEnv argumentIdents values declarations ident returnValueDim env'
        finalEnv <- transCompoundStatement compoundStmnt env''
        let val = getVarVal finalEnv ident
        case finalEnv of
          (localEnv : env) -> do
            return (val, env)
      _ -> error ("Procedures don't return values")

-- Executes the for loop, ident is the identyficator of the counter variable,
-- endValue is the value which causes the loop to finish when counter reaches
-- it.
executeForStatement :: Ident -> Value -> Statement -> GEnv -> IO GEnv
executeForStatement ident endValue statement env = do
  let i = getVarVal env ident
  case (i, endValue) of
    (VInt x, VInt y) ->
      if i > endValue then return env
      else do
        env' <- transStatement statement env
        let env'' = setVarVal env' ident (VInt(x + 1))
        executeForStatement ident endValue statement env''
    _ -> error ("For statement can only be used with expressions that " ++
                "evaluate to integers")

transForStatement :: ForStatement -> GEnv -> IO GEnv
transForStatement x env = case x of
  ForStmnt ident expression1 expression2 statement -> do
    (val1, env') <- transExpression expression1 env
    (val2, env'') <- transExpression expression2 env'
    let env''' = setVarVal env'' ident val1
    executeForStatement ident val2 statement env'''
    
transWhileStatement :: WhileStatement -> GEnv -> IO GEnv
transWhileStatement x env = case x of
  WhileStmnt expression statement -> do
    (val, env') <- transExpression expression env
    case val of
      VBool(True) -> do
        env'' <- transStatement statement env'
        transWhileStatement x env''
      VBool(False) -> return env'
      _ -> error ("While statement can only be used with expressions that " ++
                  "evaluate to boolean values")

transIfStatement :: IfStatement -> GEnv -> IO GEnv
transIfStatement x env = case x of
  IfStmnt expression statement -> do
    (val, env') <- transExpression expression env
    case val of
      VBool(True) -> do
        transStatement statement env'
      VBool(False) -> return env'
      _ -> error ("If statement can only be used with expressions that " ++
                  "evaluate to boolean values")
  IfStmntWithElse expression statement1 statement2 -> do
    (val, env') <- transExpression expression env
    case val of
      VBool(True) -> do
        transStatement statement1 env'
      VBool(False) -> do
        transStatement statement2 env'
      _ -> error ("If statement can only be used with expressions that " ++
                  "evaluate to boolean values")

transPrintStatement :: PrintStatement -> GEnv -> IO GEnv
transPrintStatement x env = case x of
  PrintStmnt expression -> do
    (val, env') <- transExpression expression env
    print val
    return env'

-- Given two simple expressions, environment and a comparison function,
-- evaluates them and returns the result of the function applied to the
-- values and returns it with the new environment in context.
compareExpressions :: (SimpleExpression -> SimpleExpression -> GEnv ->
                       (Value -> Value -> Bool) -> IO (Value, GEnv))
compareExpressions simExp1 simExp2 env comparer = do
  (val1, env') <- transSimpleExpression simExp1 env
  (val2, env'') <- transSimpleExpression simExp2 env'
  return (VBool(comparer val1 val2), env'')

transExpression :: Expression -> GEnv -> IO (Value, GEnv)
transExpression x env = case x of
  ExpSimple simpleExpr ->
    transSimpleExpression simpleExpr env
  ExpEqual simpleExpr1 simpleExpr2 ->
    (compareExpressions simpleExpr1 simpleExpr2 env (==))
  ExpNotEqual simpleExpr1 simpleExpr2 ->
    (compareExpressions simpleExpr1 simpleExpr2 env (/=))
  ExpLess simpleExpr1 simpleExpr2 ->
    (compareExpressions simpleExpr1 simpleExpr2 env (<))
  ExpLessOrEqual simpleExpr1 simpleExpr2 ->
    (compareExpressions simpleExpr1 simpleExpr2 env (<=))
  ExpGreater simpleExpr1 simpleExpr2 ->
    (compareExpressions simpleExpr1 simpleExpr2 env (>))
  ExpGreaterOrEqual simpleExpr1 simpleExpr2 ->
    (compareExpressions simpleExpr1 simpleExpr2 env (>=))

-- Given simple expression, term and a binary function on ints, evaluates them
-- and return the result of the function and new environment in context. 
computeSimpleExpression :: (SimpleExpression -> Term -> (Int -> Int -> Int) ->
                            GEnv -> IO (Value, GEnv))
computeSimpleExpression simpleExpr term fun env = do
  (val1, env') <- transSimpleExpression simpleExpr env
  (val2, env'') <- transTerm term env'
  case (val1, val2) of
      (VInt x, VInt y) -> return (VInt(fun x y), env'')
      _ -> error ("Can only add/substract integers")

transSimpleExpression :: SimpleExpression -> GEnv -> IO (Value, GEnv)
transSimpleExpression x env = case x of
  SimpleExpTerm term -> transTerm term env
  SimpleExpAdd simpleExpr term ->
    computeSimpleExpression simpleExpr term (+) env
  SimpleExpSubst simpleExpr term -> do
    computeSimpleExpression simpleExpr term (-) env

transTerm :: Term -> GEnv -> IO (Value, GEnv)
transTerm x env = case x of
  TermFactor factor -> transFactor factor env
  TermMultiply term factor -> do
    (val1, env') <- transTerm term env
    (val2, env'') <- transFactor factor env'
    case (val1, val2) of
        (VInt x, VInt y) -> return (VInt(x * y), env'')
        _ -> error ("Can only multipy integers")
  TermDivide term factor -> do
    (val1, env') <- transTerm term env
    (val2, env'') <- transFactor factor env'
    case (val1, val2) of
        (VInt x, VInt y) -> if y /=0 then return (VInt(div x y), env'')
                            else error ("Can't divide by 0")
        _ -> error ("Can only divide integers")


-- Given factor, int and environment, evaluates the factor and returns its value
-- multiplied by the integer in context.
computeTransFactor :: Factor -> Int -> GEnv -> IO (Value, GEnv)
computeTransFactor factor int env = do
  (val, env') <- transFactor factor env
  case val of
      VInt x -> return(VInt(int * x), env')
      _ -> error ("Can only add '+'/'-' before integers")

-- Given a list of values, attempts to extract their int values to a list.
evaluateToInts :: [Value] -> [Int]
evaluateToInts [] = []
evaluateToInts (val : vals) = case val of
  VInt int -> if int >= 0 then (int : evaluateToInts vals)
              else error ("Arrays are indexed by nonzero ints.")
  _ -> error ("Arrays are indexed only by ints.")

transFactor :: Factor -> GEnv -> IO (Value, GEnv)
transFactor x env = case x of
  FactorExpression expression -> transExpression expression env
  FactorPlus factor -> computeTransFactor factor 1 env
  FactorMinus factor -> computeTransFactor factor (-1) env
  FactorFunctionCall functionCall -> transFunctionCall functionCall env
  FactorConstant constant -> return (transConstant constant, env)
  FactorIdent ident -> return (getVarVal env ident, env)
  FactorArray ident expressionList -> do
    let expressions = transExpressionList expressionList
    (values, env') <- evaluateArguments expressions env
    let dims = evaluateToInts values
    return (getArrayVal env' ident dims, env')
  FactorStoI expression -> do
    (val, env') <- transExpression expression env
    case val of
      (VStr str) -> return (VInt (read str :: Int), env')
      _ -> error ("Can only use 'string_to_int' on strings")
  FactorItoS expression -> do
    (val, env') <- transExpression expression env
    case val of
      (VInt val) -> return (VStr (show val), env')
      _ -> error ("Can only use 'int_to_string' on integers")

transActuals :: Actuals -> [Expression]
transActuals x = case x of
  Act expressionList -> transExpressionList expressionList

transExpressionList :: ExpressionList -> [Expression]
transExpressionList x = case x of
  ExpListEmpty -> []
  ExpListOne expression -> [expression]
  ExpList expression expressionList ->
    [expression] ++ transExpressionList expressionList

transTypeSpecifier :: TypeSpecifier -> [Int]
transTypeSpecifier x = case x of
  TypeSpecInt -> []
  TypeSpecBool -> []
  TypeSpecString -> []
  TypeSpecArray dimensionList typeSpecifier ->
    transDimensionList dimensionList ++ transTypeSpecifier typeSpecifier

transDimensionList :: DimensionList -> [Int]
transDimensionList x = case x of
  DimListEnd integer -> [(fromInteger integer)]
  DimList integer dimensionList ->
    ((fromInteger integer) : transDimensionList dimensionList)

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

getIdentsFromProcHeader :: ProcHeader -> [Ident]
getIdentsFromProcHeader x = case x of
  ProcHead ident arguments -> getIdentsFromArguments arguments

getIdentsFromFuncHeader :: FuncHeader -> [Ident]
getIdentsFromFuncHeader x = case x of
  FunHead ident arguments typeSpecifier -> getIdentsFromArguments arguments

getIdentsFromArguments :: Arguments -> [Ident]
getIdentsFromArguments x = case x of
  Args argumentList -> getIdentsFromArgList argumentList

getIdentsFromArgList :: ArgumentList -> [Ident]
getIdentsFromArgList x = case x of
  ArgListEmpty -> []
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
  FunHead ident arguments typeSpecifier -> transTypeSpecifier typeSpecifier
