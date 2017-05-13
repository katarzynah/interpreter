module Interpreter where

import qualified Data.Map as Map

import AbsInterpreter
import ErrM

data Value = VInt Integer | VBool Bool | VStr String | VArray [Value] | VNull

instance Show Value where
    show (VInt val) = show val
    show (VBool val) = show val
    show (VStr val) = show val
    show (VArray val) = show val
    show VNull = "undefined"

instance Eq Value where
    (VInt val1) == (VInt val2) = val1 == val2
    (VBool val1) == (VBool val2) = val1 == val2
    (VStr val1) == (VStr val2) = val1 == val2

instance Ord Value where
    compare (VInt val1) (VInt val2) = compare val1 val2
    compare (VBool val1) (VBool val2) = compare val1 val2
    compare (VStr val1) (VStr val2) = compare val1 val2

-- Type mapping variable identificators to their values.
type VEnv = Map.Map Ident Value

-- Type mapping function/procedure identificators to their declarations.
type PEnv = Map.Map Ident (ProcDec)

-- Type containing both of the above.
type Env = (VEnv, PEnv)

-- Type containing list of environments - each next index is the next embedded
-- level of declarations. It keeps the levels in revers order, so that the most
-- global declarations are at the end of the list.
type GEnv = [Env]

emptyEnv :: Env
emptyEnv = (Map.empty, Map.empty)

emptyGEnv :: GEnv
emptyGEnv = [emptyEnv]

transProgram :: Program -> IO()
transProgram x = case x of
  Prog _ declarations compoundStmt -> do
    env <- transDeclarations declarations emptyGEnv
    print env --TODO: used in tests!!! but remove before turning in
    env' <- transCompoundStatement compoundStmt env
    return ()

transDeclarations :: Declarations -> GEnv -> IO GEnv
transDeclarations x env = case x of
  Dec varDeclarations procDeclarations -> do
    env' <- transVariableDeclarations varDeclarations env
    env'' <- transProcedureDeclarations procDeclarations env'
    return env''

transVariableDeclarations :: VariableDeclarations -> GEnv -> IO GEnv
transVariableDeclarations x env = case x of
  VarDecEmpty -> do return env
  VarDecFull varDeclarationList -> do
    env' <- transVariableDeclarationList varDeclarationList env
    return env'

transVariableDeclarationList :: VariableDeclarationList -> GEnv -> IO GEnv
transVariableDeclarationList x env = case x of
  VarDecListEnd varDecl -> do
    env' <- transVarDec varDecl env
    return env'
  VarDecList varDecl varDeclarationlist -> do
    env' <- transVarDec varDecl env
    env'' <- transVariableDeclarationList varDeclarationlist env'
    return env''

transVarDec :: VarDec -> GEnv -> IO GEnv
transVarDec x env = case x of
  VarDecLabel idList typeSpecifier -> do
    let dimensions = transTypeSpecifier typeSpecifier
    return (declareVars env (transIdList idList) dimensions)
    --case dimensions of
    --  [] -> return (declareVars env (transIdList idList))
    --  _ -> return (declareArrays env (transIdList idList) dimensions)
    
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

-- TODO(Kasia): Simplify if not changed
transProcDec :: ProcDec -> GEnv -> IO GEnv
transProcDec x env = case x of
  ProcDecProc procHeader declarations compoundStmnt -> do
    let env' = setDecl env (getProcDecIdent x) x
    return env'
  ProcDecFun funcHeader declarations compoundStmnt -> do
    let env' = setDecl env (getProcDecIdent x) x
    return env'

getIdentsAndDimsFromProcHeader :: ProcHeader -> [([Ident], [Integer])]
getIdentsAndDimsFromProcHeader x = case x of
  ProcHead ident arguments -> getIdentsAndDimsFromArguments arguments

getIdentsAndDimsFromFuncHeader :: FuncHeader -> [([Ident], [Integer])]
getIdentsAndDimsFromFuncHeader x = case x of
  FunHead ident arguments typeSpecifier -> getIdentsAndDimsFromArguments arguments

getIdentsAndDimsFromArguments :: Arguments -> [([Ident], [Integer])]
getIdentsAndDimsFromArguments x = case x of
  Args argumentList -> getIdentsAndDimsFromArgList argumentList

getIdentsAndDimsFromArgList :: ArgumentList -> [([Ident], [Integer])]
getIdentsAndDimsFromArgList x = case x of
  ArgListEmpty -> []
  ArgList arg argumentList ->
    [getIdentsAndDimsFromArg arg] ++ getIdentsAndDimsFromArgList argumentList

getIdentsAndDimsFromArg :: Arg -> ([Ident], [Integer])
getIdentsAndDimsFromArg x = case x of
  ArgLabel idList typeSpecifier ->
    (transIdList idList, transTypeSpecifier typeSpecifier)

-- TODO(Kasia): Add variable declarations in blacks?
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
    let env'' = setVarVal env' ident val
    return env''

_evaluateArguments :: [Expression] -> GEnv -> [Value] -> IO([Value], GEnv)
_evaluateArguments [] env values = do
  return(values, env)
_evaluateArguments (expr : exprs) env values = do
  (val, env') <- transExpression expr env
  _evaluateArguments exprs env' (values ++ [val])

evaluateArguments :: [Expression] -> GEnv -> IO([Value], GEnv)
evaluateArguments exprs env = _evaluateArguments exprs env []

getArgumentIdents :: [([Ident], [Integer])] -> [Ident]
getArgumentIdents [] = []
getArgumentIdents ((idents, _) : xs) = idents ++ getArgumentIdents xs

transProcedureCall :: ProcedureCall -> GEnv -> IO GEnv
transProcedureCall x env = case x of
  ProcCall ident actuals -> do
    let procDec = getDecl env ident
    let arguments = transActuals actuals
    (values, env') <- evaluateArguments arguments env
    case procDec of
      (ProcDecProc procHeader declarations compoundStmnt) -> do
        let argumentIdentsWithDims = getIdentsAndDimsFromProcHeader procHeader
        let argumentIdents = getArgumentIdents argumentIdentsWithDims
        if (length values) /= (length argumentIdents) then
          error ("Wrong number of arguments")
        else do
          -- adding a new local environment to our environments
          let newEnv = ((emptyEnv) : env')
          -- declaring procedure arguments in the environment
          let newEnvWithUndefinedArgs = declareArguments newEnv argumentIdentsWithDims
          -- setting the values of arguments in the environment
          let newEnvWithArgs = setVarsVals newEnvWithUndefinedArgs argumentIdents values
          -- adding local variable declarations
          newEnvWithArgsAndVars <- transDeclarations declarations newEnvWithArgs
          finalEnv <- transCompoundStatement compoundStmnt newEnvWithArgsAndVars
          case finalEnv of
            (localEnv : env) -> do return env

transFunctionCall :: FunctionCall -> GEnv -> IO (Value, GEnv)
transFunctionCall x env = case x of
  FunsCall ident actuals -> do
    let funcDec = getDecl env ident
    let arguments = transActuals actuals
    (values, env') <- evaluateArguments arguments env
    case funcDec of
      (ProcDecFun funcHeader declarations compoundStmnt) -> do
        let argumentIdentsWithDims = getIdentsAndDimsFromFuncHeader funcHeader
        let argumentIdents = getArgumentIdents argumentIdentsWithDims
        if (length values) /= (length argumentIdents) then
          error ("Wrong number of arguments")
        else do
          let newEnv = ((emptyEnv) : env')
          let newEnvWithUndefinedArgs = declareArguments newEnv argumentIdentsWithDims
          let newEnvWithArgs = setVarsVals newEnvWithUndefinedArgs argumentIdents values
          newEnvWithArgsAndVars <- transDeclarations declarations newEnvWithArgs
          let newEnvWithResult = declareVar newEnvWithArgsAndVars ident []
          finalEnv <- transCompoundStatement compoundStmnt newEnvWithResult
          case finalEnv of
            (localEnv : env) -> do
              let val = getVarVal finalEnv ident
              return (val, env)
      _ -> error ("Procedures don't return values")

-- Executes the for loop, ident is the identyficator of the counter variavle,
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
        let env'' = setVarVal env' ident (VInt(x+1))
        executeForStatement ident endValue statement env''
    _ -> error ("For statement can only be used with expressions that evaluate to integers")

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
      VBool(False) -> return (env')
      _ -> error ("While statement can only be used with expressions that evaluate to boolean values")

transIfStatement :: IfStatement -> GEnv -> IO GEnv
transIfStatement x env = case x of
  IfStmnt expression statement -> do
    (val, env') <- transExpression expression env
    case val of
      VBool(True) -> do
        transStatement statement env'
      VBool(False) -> return (env')
      _ -> error ("If statement can only be used with expressions that evaluate to boolean values")
  IfStmntWithElse expression statement1 statement2 -> do
    (val, env') <- transExpression expression env
    case val of
      VBool(True) -> do
        transStatement statement1 env'
      VBool(False) -> do
        transStatement statement2 env'
      _ -> error ("If statement can only be used with expressions that evaluate to boolean values")

transPrintStatement :: PrintStatement -> GEnv -> IO GEnv
transPrintStatement x env = case x of
  PrintStmnt expression -> do
    (val, env') <- transExpression expression env
    print val
    return (env')

compareExpressions :: SimpleExpression -> SimpleExpression -> GEnv -> (Value -> Value -> Bool) -> IO (Value, GEnv)
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

computeSimpleExpression :: SimpleExpression -> Term -> (Integer -> Integer -> Integer) -> GEnv -> IO (Value, GEnv)
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

computeTransFactor :: Factor -> Integer -> GEnv -> IO (Value, GEnv)
computeTransFactor factor int env = do
  (val, env') <- transFactor factor env
  case val of
      VInt x -> return(VInt(int * x), env')
      _ -> error ("Can only add '+'/'-' before integers")

checkIfValuesInt :: [Value] -> [Integer]
checkIfValuesInt [] = []
checkIfValuesInt (val : vals) = case val of
  VInt int -> (int : checkIfValuesInt vals)
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
    let checkedValues = checkIfValuesInt values
    return (getArrayVal env' ident checkedValues, env')
  FactorStoI expression -> do
    (val, env') <- transExpression expression env
    case val of
      (VStr str) -> return (VInt(read str :: Integer), env')
      _ -> error ("Can only use 'string_to_int' on strings")
  FactorItoS expression -> do
    (val, env') <- transExpression expression env
    case val of
      (VInt val) -> return (VStr(show val), env')
      _ -> error ("Can only use 'int_to_string' on integers")

transActuals :: Actuals -> [Expression]
transActuals x = case x of
  Act expressionList -> transExpressionList expressionList

transExpressionList :: ExpressionList -> [Expression]
transExpressionList x = case x of
  ExpListEmpty -> []
  ExpList expression expressionList -> [expression] ++ transExpressionList expressionList

transIdList :: IdList -> [Ident]
transIdList x = case x of
  IdLEnd ident -> [ident]
  IdL ident idList -> [ident] ++ transIdList idList

transTypeSpecifier :: TypeSpecifier -> [Integer]
transTypeSpecifier x = case x of
  TypeSpecInt -> []
  TypeSpecBool -> []
  TypeSpecString -> []
  TypeSpecArray dimensionList typeSpecifier ->
    transDimensionList dimensionList ++ transTypeSpecifier typeSpecifier

transDimensionList :: DimensionList -> [Integer]
transDimensionList x = case x of
  DimListEnd integer -> [integer]
  DimList integer dimensionList -> (integer : transDimensionList dimensionList)

transConstant :: Constant -> Value
transConstant x = case x of
  ConstInt integer -> VInt integer
  ConstBool boolean -> transBoolean boolean
  ConstString string -> VStr string

transBoolean :: Boolean -> Value
transBoolean x = case x of
  BoolTrue -> VBool(True)
  BoolFalse -> VBool(False)


-- GEnv helper functions

-- Given int and a value returns VArray ([value] * n).
_getArrayOfValues :: Integer -> Value -> Value
_getArrayOfValues n val | n <= 0 = error ("Array dimensions must be positive integers")
                        | otherwise = VArray (take (fromInteger n) (repeat val))

-- Returns matrix of given dimenensions of VNull.
_getValueToDeclare :: [Integer] -> Value
_getValueToDeclare [] = VNull
_getValueToDeclare (int : ints) = _getArrayOfValues int (_getValueToDeclare ints)

-- Add variable identifier to the most local environment with unspecified value.
declareVar :: GEnv -> Ident -> [Integer] -> GEnv
declareVar ((localVEnv, localPEnv) : envs) ident dims =
  case Map.lookup ident localVEnv of
    Nothing -> ((Map.insert ident value localVEnv, localPEnv) : envs) where
      value = _getValueToDeclare dims
    Just _ -> error("Variable " ++ show ident ++ " already declared")

-- Add a list of variable identifiers to the most local environment with
-- unspecified value.
declareVars :: GEnv -> [Ident] -> [Integer] -> GEnv
declareVars env [] _ = env
declareVars env (ident : idents) dims =
  declareVars (declareVar env ident dims) idents dims

declareArguments :: GEnv -> [([Ident], [Integer])] -> GEnv
declareArguments env [] = env
declareArguments env ((idents, dims) : xs) =
  declareArguments (declareVars env idents dims) xs

-- Set value of already declared variable with given identifier.
setVarVal :: GEnv -> Ident -> Value -> GEnv
setVarVal [] ident val = error("Variable " ++ show ident ++ " not defined")
setVarVal ((localVEnv, localPEnv) : envs) ident val =
  case Map.lookup ident localVEnv of
    Nothing -> ((localVEnv, localPEnv) : setVarVal envs ident val)
    Just _ -> ((Map.insert ident val localVEnv, localPEnv) : envs)

-- Set values of a list of idents. The lists must be of same length!
setVarsVals :: GEnv -> [Ident] -> [Value] -> GEnv
setVarsVals env [] _ = env
setVarsVals env (ident : idents) (val : vals) =
  setVarsVals (setVarVal env ident val) idents vals

_getValueToFetch :: Value -> [Integer] -> Value
_getValueToFetch val [] = val
_getValueToFetch val (dim : dims) = case val of
  VArray values -> _getValueToFetch (values!!(fromInteger dim)) dims
  _ -> error ("Wrong number of dimensions provided!")

-- Get value of variable with given identifier.
getVarVal :: GEnv -> Ident -> Value
getVarVal [] ident = error("Variable " ++ show ident ++ " not defined")
getVarVal ((localVEnv, _) : envs) ident =
  case Map.lookup ident localVEnv of
    Nothing -> getVarVal envs ident
    Just val -> val

getArrayVal :: GEnv -> Ident -> [Integer] -> Value
getArrayVal [] ident _ = error("Variable " ++ show ident ++ " not defined")
getArrayVal ((localVEnv, _) : envs) ident dims =
  case Map.lookup ident localVEnv of
    Nothing -> getVarVal envs ident
    Just val -> _getValueToFetch val dims

-- Add function/procedure identifier to the most local environment.
setDecl :: GEnv -> Ident -> ProcDec -> GEnv
setDecl ((localVEnv, localPEnv) : envs) ident procDec =
  case Map.lookup ident localPEnv of
    Nothing -> ((localVEnv, Map.insert ident procDec localPEnv) : envs)
    Just _ -> error("Function/procedure " ++ show ident ++ " already defined")

-- Get definition of function/procedure with given identifier.
getDecl :: GEnv -> Ident -> ProcDec
getDecl [] ident = error("Function/procedure " ++ show ident ++ " not defined")
getDecl ((_, localPEnv) : envs) ident =
  case Map.lookup ident localPEnv of
    Nothing -> getDecl envs ident
    Just val -> val
