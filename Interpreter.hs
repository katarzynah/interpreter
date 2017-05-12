module Interpreter where

import qualified Data.Map as Map

import AbsInterpreter
import ErrM
type Result = Err String

data Value = VInt Integer | VBool Bool | VString String | VUndef

instance Show Value where
    show (VInt val)       = show val
    show (VBool val)      = show val
    show (VString val)    = show val
    show VUndef         = "undefined"

instance Eq Value where
    (VInt val1) == (VInt val2) = val1 == val2
    (VBool val1) == (VBool val2) = val1 == val2
    (VString val1) == (VString val2) = val1 == val2

instance Ord Value where
    compare (VInt val1) (VInt val2) = compare val1 val2
    compare (VBool val1) (VBool val2) = compare val1 val2
    compare (VString val1) (VString val2) = compare val1 val2

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

failure :: Show a => a -> Result
failure x = Bad $ "Undefined case: " ++ show x

transProgram :: Program -> IO()
transProgram x = case x of
  Prog programheader declarations compoundstatement -> do
    env <- transDeclarations declarations emptyGEnv
    print env --TODO: used in tests!!! but remove before turning in
    env' <- transCompoundStatement compoundstatement env
    return ()

transProgramHeader :: ProgramHeader -> Result
transProgramHeader x = case x of
  ProgHead ident -> failure x

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
  VarDecLabel idlist typespecifier ->
    return (declareVars env (transIdList idlist))
    
transProcedureDeclarations :: ProcedureDeclarations -> GEnv -> IO GEnv
transProcedureDeclarations x env = case x of
  ProcDecEmpty -> return env
  ProcDecLabel procdec proceduredeclarations -> do
    env' <- transProcDec procdec env
    transProcedureDeclarations proceduredeclarations env'

-- Gets identifier from procedure/function declaration.
getProcDecIdent :: ProcDec -> Ident
getProcDecIdent (ProcDecProc (ProcHead id _) _ _) = id
getProcDecIdent (ProcDecFun (FunHead id _ _) _ _) = id

-- TODO(Kasia): Simplify if not changed
transProcDec :: ProcDec -> GEnv -> IO GEnv
transProcDec x env = case x of
  ProcDecProc procheader declarations compoundstatement -> do
    return(setDecl env (getProcDecIdent x) x)
  ProcDecFun funcheader declarations compoundstatement -> do
    return(setDecl env (getProcDecIdent x) x)

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

transCompoundStatement :: CompoundStatement -> GEnv -> IO GEnv
transCompoundStatement x env = case x of
  CompStmnt statementlist -> transStatementList statementlist env

transStatementList :: StatementList -> GEnv -> IO GEnv
transStatementList x env = case x of
  StmntListEmpty -> return env
  StmntList statement statementlist -> do
    env' <- transStatement statement env
    transStatementList statementlist env'

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
    return (setVarVal env' ident value)

_evaluateArguments :: [Expression] -> GEnv -> [Value] -> IO([Value], GEnv)
_evaluateArguments [] env values = do
  return(values, env)
_evaluateArguments (expr : exprs) env values = do
  (val, env') <- transExpression expr env
  _evaluateArguments exprs env' (values ++ [val])

evaluateArguments :: [Expression] -> GEnv -> IO([Value], GEnv)
evaluateArguments exprs env = _evaluateArguments exprs env []

-- TODO(Kasia): Since I have support for any embedded functions, I should add them.
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
          let newEnvWithUndefinedArgs = declareVars newEnv argumentIdents
          -- setting the values of arguments in the environment
          let newEnvWithArgs = setVarsVals newEnvWithUndefinedArgs argumentIdents values
          -- adding local variable declarations
          newEnvWithArgsAndVars <- transDeclarations declarations newEnvWithArgs
          finalEnv <- transCompoundStatement compoundstatement newEnvWithArgsAndVars
          case finalEnv of
            (localEnv : env) -> do return(env)

transFunctionCall :: FunctionCall -> GEnv -> IO (Value, GEnv)
transFunctionCall x env = case x of
  FunsCall ident actuals -> do
    let funcDec = getDecl env ident
    let arguments = transActuals actuals
    (values, env') <- evaluateArguments arguments env
    case funcDec of
      (ProcDecFun funcheader declarations compoundstatement) -> do
        let argumentIdents = transFuncHeader funcheader
        if (length values) /= (length argumentIdents) then error ("Wrong number of arguments")
        else do
          let newEnv = ((emptyEnv) : env')
          let newEnvWithUndefinedArgs = declareVars newEnv argumentIdents
          let newEnvWithArgs = setVarsVals newEnvWithUndefinedArgs argumentIdents values
          newEnvWithArgsAndVars <- transDeclarations declarations newEnvWithArgs
          let newEnvWithResult = declareVar newEnvWithArgsAndVars ident
          finalEnv <- transCompoundStatement compoundstatement newEnvWithResult
          case finalEnv of
            (localEnv : env) -> do return(getVarVal finalEnv ident, env)
      _ -> error ("Procedures don't return values")

executeForStatement :: Ident -> Value -> Statement -> GEnv -> IO GEnv
executeForStatement ident endValue statement env = do
  let i = getVarVal env ident
  case (i, endValue) of
    (VInt x, VInt y) ->
      if i > endValue then return env
      else do
        env' <- transStatement statement env
        executeForStatement ident endValue statement $ setVarVal env' ident (VInt(x+1))
    _ -> error ("For statement can only be used with expressions that evaluate to integers")

transForStatement :: ForStatement -> GEnv -> IO GEnv
transForStatement x env = case x of
  ForStmnt ident expression1 expression2 statement -> do
    (val1, env') <- transExpression expression1 env
    (val2, env'') <- transExpression expression2 env'
    executeForStatement ident val2 statement $ setVarVal env'' ident val1
    
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
  ExpSimple simpleexpression -> transSimpleExpression simpleexpression env
  ExpEqual simpleexpression1 simpleexpression2 -> (compareExpressions simpleexpression1 simpleexpression2 env (==))
  ExpNotEqual simpleexpression1 simpleexpression2 -> (compareExpressions simpleexpression1 simpleexpression2 env (/=))
  ExpLess simpleexpression1 simpleexpression2 -> (compareExpressions simpleexpression1 simpleexpression2 env (<))
  ExpLessOrEqual simpleexpression1 simpleexpression2 -> (compareExpressions simpleexpression1 simpleexpression2 env (<=))
  ExpGreater simpleexpression1 simpleexpression2 -> (compareExpressions simpleexpression1 simpleexpression2 env (>))
  ExpGreaterOrEqual simpleexpression1 simpleexpression2 -> (compareExpressions simpleexpression1 simpleexpression2 env (>=))

transSimpleExpression :: SimpleExpression -> GEnv -> IO (Value, GEnv)
transSimpleExpression x env = case x of
  SimpleExpTerm term -> transTerm term env
  SimpleExpAdd simpleexpression term -> do
    (val1, env') <- transSimpleExpression simpleexpression env
    (val2, env'') <- transTerm term env'
    case (val1, val2) of
        (VInt x, VInt y) -> return (VInt(x + y), env'')
        _ -> error ("Can only add integers")
  SimpleExpSubst simpleexpression term -> do
    (val1, env') <- transSimpleExpression simpleexpression env
    (val2, env'') <- transTerm term env'
    case (val1, val2) of
        (VInt x, VInt y) -> return (VInt(x - y), env'')
        _ -> error ("Can only substract integers")

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

transFactor :: Factor -> GEnv -> IO (Value, GEnv)
transFactor x env = case x of
  FactorExpression expression -> transExpression expression env
  FactorPlus factor -> do
    (val, env') <- transFactor factor env
    case val of
        VInt x -> return(VInt(x), env')
        _ -> error ("Can only add '+' before integers")
  FactorMinus factor -> do
    (val, env') <- transFactor factor env
    case val of
        VInt x -> return(VInt(-x), env')
        _ -> error ("Can only add '-' before integers")
  FactorFunctionCall functioncall -> transFunctionCall functioncall env
  FactorConstant constant -> return (transConstant constant, env)
  FactorIdent ident -> do
    return (getVarVal env ident, env)
  FactorStoI expression -> do
    (val, env') <- transExpression expression env
    case val of
      (VString str) -> return (VInt(read str :: Integer), env')
      _ -> error ("Can only use 'string_to_int' on strings")
  FactorItoS expression -> do
    (val, env') <- transExpression expression env
    case val of
      (VInt val) -> return (VString(show val), env')
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

transIdent :: Ident -> Result
transIdent x = case x of
  Ident string -> failure x

transTypeSpecifier :: TypeSpecifier -> Result
transTypeSpecifier x = case x of
  TypeSpecInt -> failure x
  TypeSpecBool -> failure x
  TypeSpecString -> failure x
  TypeSpecArray dimensionlist typespecifier -> failure x

transDimensionList :: DimensionList -> Result
transDimensionList x = case x of
  DimListEnd dimension -> failure x
  DimList dimension dimensionlist -> failure x

transDimension :: Dimension -> Result
transDimension x = case x of
  DimId ident -> failure x
  DimConst constant -> failure x

transConstant :: Constant -> Value
transConstant x = case x of
  ConstInt integer -> VInt integer
  ConstBool boolean -> transBoolean boolean
  ConstString string -> VString string

transBoolean :: Boolean -> Value
transBoolean x = case x of
  BoolTrue -> VBool(True)
  BoolFalse -> VBool(False)


-- GEnv helper functions

-- Add variable identifier to the most local environment with unspecified value.
declareVar :: GEnv -> Ident -> GEnv
declareVar ((localVEnv, lovalPEnv) : envs) ident =
  case Map.lookup ident localVEnv of
    Nothing -> ((Map.insert ident VUndef localVEnv, lovalPEnv) : envs)
    Just _ -> error("Variable " ++ show ident ++ " already declared")

-- Add a list of variable identifiers to the most local environment with
-- unspecified value.
declareVars :: GEnv -> [Ident] -> GEnv
declareVars env [] = env
declareVars env (ident : idents) = declareVars (declareVar env ident) idents

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
getVarVal ((localVEnv, lovalPEnv) : envs) ident =
  case Map.lookup ident localVEnv of
    Nothing -> getVarVal envs ident
    Just val -> val

-- Add function/procedure identifier to the most local environment.
setDecl :: GEnv -> Ident -> ProcDec -> GEnv
setDecl ((localVEnv, lovalPEnv) : envs) ident procDec =
  case Map.lookup ident lovalPEnv of
    Nothing -> ((localVEnv, Map.insert ident procDec lovalPEnv) : envs)
    Just _ -> error("Function/procedure " ++ show ident ++ " already defined")

-- Get definition of function/procedure with given identifier.
getDecl :: GEnv -> Ident -> ProcDec
getDecl [] ident = error("Function/procedure " ++ show ident ++ " not defined")
getDecl ((localVEnv, lovalPEnv) : envs) ident =
  case Map.lookup ident lovalPEnv of
    Nothing -> getDecl envs ident
    Just val -> val
