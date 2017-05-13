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
  Prog programheader declarations compoundstatement -> do
    env <- transDeclarations declarations emptyGEnv
    print env --TODO: used in tests!!! but remove before turning in
    env' <- transCompoundStatement compoundstatement env
    return ()

transDeclarations :: Declarations -> GEnv -> IO GEnv
transDeclarations x env = case x of
  Dec variabledeclarations proceduredeclarations -> do
    env' <- transVariableDeclarations variabledeclarations env
    env'' <- transProcedureDeclarations proceduredeclarations env'
    return env''

transVariableDeclarations :: VariableDeclarations -> GEnv -> IO GEnv
transVariableDeclarations x env = case x of
  VarDecEmpty -> do return env
  VarDecFull variabledeclarationlist -> do
    env' <- transVariableDeclarationList variabledeclarationlist env
    return env'

transVariableDeclarationList :: VariableDeclarationList -> GEnv -> IO GEnv
transVariableDeclarationList x env = case x of
  VarDecListEnd vardec -> do
    env' <- transVarDec vardec env
    return env'
  VarDecList vardec variabledeclarationlist -> do
    env' <- transVarDec vardec env
    env'' <- transVariableDeclarationList variabledeclarationlist env'
    return env''

transVarDec :: VarDec -> GEnv -> IO GEnv
transVarDec x env = case x of
  VarDecLabel idlist typespecifier -> do
    let dimensions = transTypeSpecifier typespecifier
    return (declareVars env (transIdList idlist) dimensions)
    --case dimensions of
    --  [] -> return (declareVars env (transIdList idlist))
    --  _ -> return (declareArrays env (transIdList idlist) dimensions)
    
transProcedureDeclarations :: ProcedureDeclarations -> GEnv -> IO GEnv
transProcedureDeclarations x env = case x of
  ProcDecEmpty -> return env
  ProcDecLabel procdec proceduredeclarations -> do
    env' <- transProcDec procdec env
    env'' <- transProcedureDeclarations proceduredeclarations env'
    return env''

-- Gets identifier from procedure/function declaration.
getProcDecIdent :: ProcDec -> Ident
getProcDecIdent (ProcDecProc (ProcHead id _) _ _) = id
getProcDecIdent (ProcDecFun (FunHead id _ _) _ _) = id

-- TODO(Kasia): Simplify if not changed
transProcDec :: ProcDec -> GEnv -> IO GEnv
transProcDec x env = case x of
  ProcDecProc procheader declarations compoundstatement -> do
    let env' = setDecl env (getProcDecIdent x) x
    return env'
  ProcDecFun funcheader declarations compoundstatement -> do
    let env' = setDecl env (getProcDecIdent x) x
    return env'

transProcHeader :: ProcHeader -> [Ident]
transProcHeader x = case x of
  ProcHead ident arguments -> transArguments arguments

transFuncHeader :: FuncHeader -> [Ident]
transFuncHeader x = case x of
  FunHead ident arguments typespecifier -> transArguments arguments

transArguments :: Arguments -> [Ident]
transArguments x = case x of
  Args argumentlist -> transArgumentList argumentlist

transArgumentList :: ArgumentList -> [Ident]
transArgumentList x = case x of
  ArgListEmpty -> []
  ArgList arg argumentlist -> transArg arg ++ transArgumentList argumentlist

transArg :: Arg -> [Ident]
transArg x = case x of
  ArgLabel idlist typespecifier -> transIdList idlist

-- TODO(Kasia): Add variable declarations in blacks?
transCompoundStatement :: CompoundStatement -> GEnv -> IO GEnv
transCompoundStatement x env = case x of
  CompStmnt statementlist -> transStatementList statementlist env

transStatementList :: StatementList -> GEnv -> IO GEnv
transStatementList x env = case x of
  StmntListEmpty -> return env
  StmntList statement statementlist -> do
    env' <- transStatement statement env
    env'' <- transStatementList statementlist env'
    return env''

transStatement :: Statement -> GEnv -> IO GEnv
transStatement x env = case x of
  SEmpty -> return env
  SComp compoundstatement -> transCompoundStatement compoundstatement env
  SAss assignmentstatement -> transAssignmentStatement assignmentstatement env
  SProc procedurecall -> transProcedureCall procedurecall env
  SFor forstatement -> transForStatement forstatement env
  SWhile whilestatement -> transWhileStatement whilestatement env
  SIf ifstatement -> transIfStatement ifstatement env
  SPrint printstatement -> transPrintStatement printstatement env

transAssignmentStatement :: AssignmentStatement -> GEnv -> IO GEnv
transAssignmentStatement x env = case x of
  AssStmnt ident expression -> do
    (value, env') <- transExpression expression env
    let env'' = setVarVal env' ident value
    return env''

_evaluateArguments :: [Expression] -> GEnv -> [Value] -> IO([Value], GEnv)
_evaluateArguments [] env values = do
  return(values, env)
_evaluateArguments (expr : exprs) env values = do
  (val, env') <- transExpression expr env
  _evaluateArguments exprs env' (values ++ [val])

evaluateArguments :: [Expression] -> GEnv -> IO([Value], GEnv)
evaluateArguments exprs env = _evaluateArguments exprs env []

transProcedureCall :: ProcedureCall -> GEnv -> IO GEnv
transProcedureCall x env = case x of
  ProcCall ident actuals -> do
    let procDec = getDecl env ident
    let arguments = transActuals actuals
    (values, env') <- evaluateArguments arguments env
    case procDec of
      (ProcDecProc procheader declarations compoundstatement) -> do
        let argumentIdents = transProcHeader procheader
        if (length values) /= (length argumentIdents) then error ("Wrong number of arguments")
        else do
          -- adding a new local environment to our environments
          let newEnv = ((emptyEnv) : env')
          -- declaring procedure arguments in the environment
          let newEnvWithUndefinedArgs = declareVars newEnv argumentIdents [] -- TODO: fix
          -- setting the values of arguments in the environment
          let newEnvWithArgs = setVarsVals newEnvWithUndefinedArgs argumentIdents values
          -- adding local variable declarations
          newEnvWithArgsAndVars <- transDeclarations declarations newEnvWithArgs
          finalEnv <- transCompoundStatement compoundstatement newEnvWithArgsAndVars
          case finalEnv of
            (localEnv : env) -> do return env

transFunctionCall :: FunctionCall -> GEnv -> IO (Value, GEnv)
transFunctionCall x env = case x of
  FunsCall ident actuals -> do
    let funcDec = getDecl env ident
    let arguments = transActuals actuals
    (values, env') <- evaluateArguments arguments env
    case funcDec of
      (ProcDecFun funcheader declarations compoundstatement) -> do
        let argumentIdents = transFuncHeader funcheader
        if (length values) /= (length argumentIdents) then
          error ("Wrong number of arguments")
        else do
          let newEnv = ((emptyEnv) : env')
          let newEnvWithUndefinedArgs = declareVars newEnv argumentIdents [] -- TODO: fix
          let newEnvWithArgs = setVarsVals newEnvWithUndefinedArgs argumentIdents values
          newEnvWithArgsAndVars <- transDeclarations declarations newEnvWithArgs
          let newEnvWithResult = declareVar newEnvWithArgsAndVars ident []
          finalEnv <- transCompoundStatement compoundstatement newEnvWithResult
          case finalEnv of
            (localEnv : env) -> do
              let val = getVarVal finalEnv ident
              return (val, env)
      _ -> error ("Procedures don't return values")

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
  ExpSimple simpleexpression ->
    transSimpleExpression simpleexpression env
  ExpEqual simpleexpression1 simpleexpression2 ->
    (compareExpressions simpleexpression1 simpleexpression2 env (==))
  ExpNotEqual simpleexpression1 simpleexpression2 ->
    (compareExpressions simpleexpression1 simpleexpression2 env (/=))
  ExpLess simpleexpression1 simpleexpression2 ->
    (compareExpressions simpleexpression1 simpleexpression2 env (<))
  ExpLessOrEqual simpleexpression1 simpleexpression2 ->
    (compareExpressions simpleexpression1 simpleexpression2 env (<=))
  ExpGreater simpleexpression1 simpleexpression2 ->
    (compareExpressions simpleexpression1 simpleexpression2 env (>))
  ExpGreaterOrEqual simpleexpression1 simpleexpression2 ->
    (compareExpressions simpleexpression1 simpleexpression2 env (>=))

computeSimpleExpression :: SimpleExpression -> Term -> (Integer -> Integer -> Integer) -> GEnv -> IO (Value, GEnv)
computeSimpleExpression simpExpr term fun env = do
  (val1, env') <- transSimpleExpression simpExpr env
  (val2, env'') <- transTerm term env'
  case (val1, val2) of
      (VInt x, VInt y) -> return (VInt(fun x y), env'')
      _ -> error ("Can only add/substract integers")

transSimpleExpression :: SimpleExpression -> GEnv -> IO (Value, GEnv)
transSimpleExpression x env = case x of
  SimpleExpTerm term -> transTerm term env
  SimpleExpAdd simpleexpression term ->
    computeSimpleExpression simpleexpression term (+) env
  SimpleExpSubst simpleexpression term -> do
    computeSimpleExpression simpleexpression term (-) env

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

transFactor :: Factor -> GEnv -> IO (Value, GEnv)
transFactor x env = case x of
  FactorExpression expression -> transExpression expression env
  FactorPlus factor -> computeTransFactor factor 1 env
  FactorMinus factor -> computeTransFactor factor (-1) env
  FactorFunctionCall functioncall -> transFunctionCall functioncall env
  FactorConstant constant -> return (transConstant constant, env)
  FactorIdent ident -> do
    return (getVarVal env ident, env)
  FactorArray ident expressionlist -> return (VInt(0), env)
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
  Act expressionlist -> transExpressionList expressionlist

transExpressionList :: ExpressionList -> [Expression]
transExpressionList x = case x of
  ExpListEmpty -> []
  ExpList expression expressionlist -> [expression] ++ transExpressionList expressionlist

transIdList :: IdList -> [Ident]
transIdList x = case x of
  IdLEnd ident -> [ident]
  IdL ident idlist -> [ident] ++ transIdList idlist

transTypeSpecifier :: TypeSpecifier -> [Integer]
transTypeSpecifier x = case x of
  TypeSpecInt -> []
  TypeSpecBool -> []
  TypeSpecString -> []
  TypeSpecArray dimensionlist typespecifier ->
    transDimensionList dimensionlist ++ transTypeSpecifier typespecifier

transDimensionList :: DimensionList -> [Integer]
transDimensionList x = case x of
  DimListEnd integer -> [integer]
  DimList integer dimensionlist -> (integer : transDimensionList dimensionlist)

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

_getArrayOfValues :: Integer -> Value -> Value
_getArrayOfValues n val | n <= 0 = error ("Array dimensions must be positive integers")
                        | otherwise = VArray (take (fromInteger n) (repeat val))

getValue :: [Integer] -> Value
getValue [] = VNull
getValue (int : ints) = _getArrayOfValues int (getValue ints)

-- Add variable identifier to the most local environment with unspecified value.
declareVar :: GEnv -> Ident -> [Integer] -> GEnv
declareVar ((localVEnv, localPEnv) : envs) ident dims =
  case Map.lookup ident localVEnv of
    Nothing -> ((Map.insert ident value localVEnv, localPEnv) : envs) where
      value = getValue dims
    Just _ -> error("Variable " ++ show ident ++ " already declared")

-- Add a list of variable identifiers to the most local environment with
-- unspecified value.
declareVars :: GEnv -> [Ident] -> [Integer] -> GEnv
declareVars env [] _ = env
declareVars env (ident : idents) dims =
  declareVars (declareVar env ident dims) idents dims

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

-- Get value of variable with given identifier.
getVarVal :: GEnv -> Ident -> Value
getVarVal [] ident = error("Variable " ++ show ident ++ " not defined")
getVarVal ((localVEnv, _) : envs) ident =
  case Map.lookup ident localVEnv of
    Nothing -> getVarVal envs ident
    Just val -> val

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
