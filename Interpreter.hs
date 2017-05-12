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

-- Environment variable mapping variable identificators to their values.
type VEnv = Map.Map Ident Value
-- Environment variable mapping function/procedure identificators to their
-- declarations.
type PEnv = Map.Map Ident (ProcDec)

emptyVEnv :: VEnv
emptyVEnv = Map.empty

emptyPEnv :: PEnv
emptyPEnv = Map.empty

failure :: Show a => a -> Result
failure x = Bad $ "Undefined case: " ++ show x

transProgram :: Program -> IO()
transProgram x = case x of
  Prog programheader declarations compoundstatement -> do
    (venv, penv) <- transDeclarations declarations emptyVEnv emptyPEnv
    print penv
    (venv', penv') <- transCompoundStatement compoundstatement venv penv
    return ()

transProgramHeader :: ProgramHeader -> Result
transProgramHeader x = case x of
  ProgHead ident -> failure x

transDeclarations :: Declarations -> VEnv -> PEnv -> IO (VEnv, PEnv)
transDeclarations x env penv = case x of
  Dec variabledeclarations proceduredeclarations -> do
    env' <- transVariableDeclarations variabledeclarations env
    penv' <- transProcedureDeclarations proceduredeclarations penv
    return (env', penv')

transVariableDeclarations :: VariableDeclarations -> VEnv -> IO VEnv
transVariableDeclarations x env = case x of
  VarDecEmpty -> do
    return(env)
  VarDecFull variabledeclarationlist -> do
    env' <- transVariableDeclarationList variabledeclarationlist env
    return (env')

transVariableDeclarationList :: VariableDeclarationList -> VEnv -> IO VEnv
transVariableDeclarationList x env = case x of
  VarDecListEnd vardec -> do
    env' <- transVarDec vardec env
    return (env')
  VarDecList vardec variabledeclarationlist -> do
    env' <- transVarDec vardec env
    env'' <- transVariableDeclarationList variabledeclarationlist env'
    return (env'')

transVarDec :: VarDec -> VEnv -> IO VEnv
transVarDec x env = case x of
  VarDecLabel idlist typespecifier ->
    return (transIdList idlist env)
    
transProcedureDeclarations :: ProcedureDeclarations -> PEnv -> IO PEnv
transProcedureDeclarations x penv = case x of
  ProcDecEmpty -> return penv
  ProcDecLabel procdec proceduredeclarations -> do
    penv' <- transProcDec procdec penv
    transProcedureDeclarations proceduredeclarations penv'

-- Get identifier from procedure/function declaration.
getProcDecIdent :: ProcDec -> Ident
getProcDecIdent (ProcDecProc (ProcHead id _) _ _) = id
getProcDecIdent (ProcDecFun (FunHead id _ _) _ _) = id

-- TODO(Kasia): Simplify if not changed
transProcDec :: ProcDec -> PEnv -> IO PEnv
transProcDec x penv = case x of
  ProcDecProc procheader variabledeclarations compoundstatement -> do
    return(setDecl penv (getProcDecIdent x) x)
  ProcDecFun funcheader variabledeclarations compoundstatement -> do
    return(setDecl penv (getProcDecIdent x) x)

transProcHeader :: ProcHeader -> Result
transProcHeader x = case x of
  ProcHead ident arguments -> failure x
transFuncHeader :: FuncHeader -> Result
transFuncHeader x = case x of
  FunHead ident arguments typespecifier -> failure x
transArguments :: Arguments -> Result
transArguments x = case x of
  Args argumentlist -> failure x
transArgumentList :: ArgumentList -> Result
transArgumentList x = case x of
  ArgListEmpty -> failure x
  ArgList arg argumentlist -> failure x
transArg :: Arg -> Result
transArg x = case x of
  ArgLabel idlist typespecifier -> failure x

transCompoundStatement :: CompoundStatement -> VEnv -> PEnv -> IO (VEnv, PEnv)
transCompoundStatement x env penv = case x of
  CompStmnt statementlist -> transStatementList statementlist env penv

transStatementList :: StatementList -> VEnv -> PEnv -> IO (VEnv, PEnv)
transStatementList x env penv = case x of
  StmntListEmpty -> return (env, penv)
  StmntList statement statementlist -> do
    (env', penv') <- transStatement statement env penv
    transStatementList statementlist env' penv'

transStatement :: Statement -> VEnv -> PEnv -> IO (VEnv, PEnv)
transStatement x env penv = case x of
  SEmpty -> return (env, penv)
  SComp compoundstatement -> transCompoundStatement compoundstatement env penv
  SAss assignmentstatement -> transAssignmentStatement assignmentstatement env penv
  --SProc procedurecall -> failure x
  SFor forstatement -> transForStatement forstatement env penv
  SWhile whilestatement -> transWhileStatement whilestatement env penv
  SIf ifstatement -> transIfStatement ifstatement env penv
  SPrint printstatement -> transPrintStatement printstatement env penv

transAssignmentStatement :: AssignmentStatement -> VEnv -> PEnv -> IO (VEnv, PEnv)
transAssignmentStatement x env penv = case x of
  AssStmnt ident expression -> do
    (value, env', penv') <- transExpression expression env penv
    return (setVarVal env' ident value, penv')

transProcedureCall :: ProcedureCall -> Result
transProcedureCall x = case x of
  ProcCall ident actuals -> failure x

executeForStatement :: Ident -> Value -> Statement -> VEnv -> PEnv -> IO (VEnv, PEnv)
executeForStatement ident endValue statement env penv = do
  let i = getVarVal env ident
  case (i, endValue) of
    (VInt x, VInt y) ->
      if i > endValue then return (env, penv)
      else do
        (env', penv') <- transStatement statement env penv
        executeForStatement ident endValue statement (setVarVal env' ident (VInt(x+1))) penv'

transForStatement :: ForStatement -> VEnv -> PEnv -> IO (VEnv, PEnv)
transForStatement x env penv = case x of
  ForStmnt ident expression1 expression2 statement -> do
    (val1, env', penv') <- transExpression expression1 env penv
    (val2, env'', penv'') <- transExpression expression2 env' penv'
    executeForStatement ident val2 statement (setVarVal env'' ident val1) penv''
    
transWhileStatement :: WhileStatement -> VEnv -> PEnv -> IO (VEnv, PEnv)
transWhileStatement x env penv = case x of
  WhileStmnt expression statement -> do
    (val, env', penv') <- transExpression expression env penv
    case val of
      VBool(True) -> do
        (env'', penv'') <- transStatement statement env' penv'
        transWhileStatement x env'' penv''
      VBool(False) -> return (env', penv')

transIfStatement :: IfStatement -> VEnv -> PEnv -> IO (VEnv, PEnv)
transIfStatement x env penv = case x of
  IfStmnt expression statement -> do
    (val, env', penv') <- transExpression expression env penv
    case val of
      VBool(True) -> do
        transStatement statement env' penv'
      VBool(False) -> return (env', penv')
  IfStmntWithElse expression statement1 statement2 -> do
    (val, env', penv') <- transExpression expression env penv
    case val of
      VBool(True) -> do
        transStatement statement1 env' penv'
      VBool(False) -> do
        transStatement statement2 env' penv'

transPrintStatement :: PrintStatement -> VEnv -> PEnv -> IO (VEnv, PEnv)
transPrintStatement x env penv = case x of
  PrintStmnt expression -> do
    (val, env', penv') <- transExpression expression env penv
    print val
    return (env', penv')

compareExpressions :: SimpleExpression -> SimpleExpression -> VEnv -> PEnv -> (Value -> Value -> Bool) -> IO (Value, VEnv, PEnv)
compareExpressions simExp1 simExp2 env penv comparer = do
  (val1, env', penv') <- transSimpleExpression simExp1 env penv
  (val2, env'', penv'') <- transSimpleExpression simExp2 env' penv'
  return (VBool(comparer val1 val2), env'', penv'')

transExpression :: Expression -> VEnv -> PEnv -> IO (Value, VEnv, PEnv)
transExpression x env penv = case x of
  ExpSimple simpleexpression -> transSimpleExpression simpleexpression env penv
  ExpEqual simpleexpression1 simpleexpression2 -> (compareExpressions simpleexpression1 simpleexpression2 env penv (==))
  ExpNotEqual simpleexpression1 simpleexpression2 -> (compareExpressions simpleexpression1 simpleexpression2 env penv (/=))
  ExpLess simpleexpression1 simpleexpression2 -> (compareExpressions simpleexpression1 simpleexpression2 env penv (<))
  ExpLessOrEqual simpleexpression1 simpleexpression2 -> (compareExpressions simpleexpression1 simpleexpression2 env penv (<=))
  ExpGreater simpleexpression1 simpleexpression2 -> (compareExpressions simpleexpression1 simpleexpression2 env penv (>))
  ExpGreaterOrEqual simpleexpression1 simpleexpression2 -> (compareExpressions simpleexpression1 simpleexpression2 env penv (>=))

transSimpleExpression :: SimpleExpression -> VEnv -> PEnv -> IO (Value, VEnv, PEnv)
transSimpleExpression x env penv = case x of
  SimpleExpTerm term -> transTerm term env penv
  SimpleExpAdd simpleexpression term -> do
    (val1, env', penv') <- transSimpleExpression simpleexpression env penv
    (val2, env'', penv'') <- transTerm term env' penv'
    case (val1, val2) of
        (VInt x, VInt y) -> return (VInt(x + y), env'', penv'')
  SimpleExpSubst simpleexpression term -> do
    (val1, env', penv') <- transSimpleExpression simpleexpression env penv
    (val2, env'', penv'') <- transTerm term env' penv'
    case (val1, val2) of
        (VInt x, VInt y) -> return (VInt(x - y), env'', penv'')

transTerm :: Term -> VEnv -> PEnv -> IO (Value, VEnv, PEnv)
transTerm x env penv = case x of
  TermFactor factor -> transFactor factor env penv
  TermMultiply term factor -> do
    (val1, env', penv') <- transTerm term env penv
    (val2, env'', penv'') <- transFactor factor env' penv'
    case (val1, val2) of
        (VInt x, VInt y) -> return (VInt(x * y), env'', penv'')
  TermDivide term factor -> do
    (val1, env', penv') <- transTerm term env penv
    (val2, env'', penv'') <- transFactor factor env' penv'
    case (val1, val2) of
        (VInt x, VInt y) -> if y /=0 then return (VInt(div x y), env'', penv'')
                            else error ("division by 0")

transFactor :: Factor -> VEnv -> PEnv -> IO (Value, VEnv, PEnv)
transFactor x env penv = case x of
  FactorExpression expression -> transExpression expression env penv
  FactorPlus factor -> transFactor factor env penv
  FactorMinus factor -> do
    (val, env', penv') <- transFactor factor env penv
    case val of
        VInt x -> return(VInt(-x), env', penv')
  --FactorFunctionCall functioncall -> failure x
  FactorConstant constant -> return (transConstant constant, env, penv)
  FactorIdent ident -> do
    return (getVarVal env ident, env, penv)
  --FactorStoI expression -> failure x
  --FactorItoS expression -> failure x
transFunctionCall :: FunctionCall -> Result
transFunctionCall x = case x of
  FunsCall ident actuals -> failure x
transActuals :: Actuals -> Result
transActuals x = case x of
  Act expressionlist -> failure x
transExpressionList :: ExpressionList -> Result
transExpressionList x = case x of
  ExpListEmpty -> failure x
  ExpList expression expressionlist -> failure x

transIdList :: IdList -> VEnv -> VEnv
transIdList x env = case x of
  IdLEnd ident -> declareVar env ident
  IdL ident idlist -> transIdList idlist $ declareVar env ident

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


-- VEnv helper functions
declareVar :: VEnv -> Ident -> VEnv
declareVar env ident =
  case Map.lookup ident env of
    Nothing -> Map.insert ident VUndef env
    Just _ -> error("Variable " ++ show ident ++ " already declared")

setVarVal :: VEnv -> Ident -> Value -> VEnv
setVarVal env ident val =
  case Map.lookup ident env of
    Nothing -> error("Variable " ++ show ident ++ " not defined")
    Just _ -> Map.insert ident val env

getVarVal :: VEnv -> Ident -> Value
getVarVal env ident =
  case Map.lookup ident env of
    Nothing -> error("Variable " ++ show ident ++ " not defined")
    Just val -> val

-- PEnv helper functions
setDecl :: PEnv -> Ident -> ProcDec -> PEnv
setDecl penv ident procDec =
  case Map.lookup ident penv of
    Nothing -> Map.insert ident procDec penv
    Just _ -> error("Function/procedure " ++ show ident ++ " already defined")

getDecl :: PEnv -> Ident -> ProcDec
getDecl penv ident =
  case Map.lookup ident penv of
    Nothing -> error("Function/procedure " ++ show ident ++ " not defined")
    Just val -> val
