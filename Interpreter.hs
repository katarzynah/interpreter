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

type VEnv = Map.Map Ident Value

emptyVEnv :: VEnv
emptyVEnv = Map.empty

failure :: Show a => a -> Result
failure x = Bad $ "Undefined case: " ++ show x

transProgram :: Program -> IO()
transProgram x = case x of
  Prog programheader declarations compoundstatement -> do
    env <- transDeclarations declarations emptyVEnv
    env' <- transCompoundStatement compoundstatement env
    return ()

transProgramHeader :: ProgramHeader -> Result
transProgramHeader x = case x of
  ProgHead ident -> failure x

transDeclarations :: Declarations -> VEnv -> IO VEnv
transDeclarations x env = case x of
  Dec variabledeclarations proceduredeclarations -> do
    env' <- transVariableDeclarations variabledeclarations env
    return (env')

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
    

transProcedureDeclarations :: ProcedureDeclarations -> Result
transProcedureDeclarations x = case x of
  ProcDecEmpty -> failure x
  ProcDecLabel procdec proceduredeclarations -> failure x
transProcDec :: ProcDec -> Result
transProcDec x = case x of
  ProcDecProc procheader variabledeclarations compoundstatement -> failure x
  ProcDecFun funcheader variabledeclarations compoundstatement -> failure x
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

transCompoundStatement :: CompoundStatement -> VEnv -> IO VEnv
transCompoundStatement x env = case x of
  CompStmnt statementlist -> transStatementList statementlist env

transStatementList :: StatementList -> VEnv -> IO VEnv
transStatementList x env = case x of
  StmntListEmpty -> return env
  StmntList statement statementlist -> do
    env' <- transStatement statement env
    transStatementList statementlist env'

transStatement :: Statement -> VEnv -> IO VEnv
transStatement x env = case x of
  SEmpty -> return env
  SComp compoundstatement -> transCompoundStatement compoundstatement env
  SAss assignmentstatement -> transAssignmentStatement assignmentstatement env
  --SProc procedurecall -> failure x
  SFor forstatement -> transForStatement forstatement env
  --SWhile whilestatement -> failure x
  --SIf ifstatement -> failure x
  SPrint printstatement -> transPrintStatement printstatement env

transAssignmentStatement :: AssignmentStatement -> VEnv -> IO VEnv
transAssignmentStatement x env = case x of
  AssStmnt ident expression -> do
    (value, env') <- transExpression expression env
    return (setVarVal env' ident value)

transProcedureCall :: ProcedureCall -> Result
transProcedureCall x = case x of
  ProcCall ident actuals -> failure x

executeForStatement :: Ident -> Value -> Statement -> VEnv -> IO VEnv
executeForStatement ident endValue statement env = do
  let i = getVarVal env ident
  case (i, endValue) of
    (VInt x, VInt y) ->
      if i > endValue then return env
      else do
        env' <- transStatement statement env
        executeForStatement ident endValue statement $ setVarVal env' ident (VInt(x+1))

transForStatement :: ForStatement -> VEnv -> IO VEnv
transForStatement x env = case x of
  ForStmnt ident expression1 expression2 statement -> do
    (val1, env') <- transExpression expression1 env
    (val2, env'') <- transExpression expression2 env'
    executeForStatement ident val2 statement $ setVarVal env ident val1
    
transWhileStatement :: WhileStatement -> Result
transWhileStatement x = case x of
  WhileStmnt expression statement -> failure x
transIfStatement :: IfStatement -> Result
transIfStatement x = case x of
  IfStmnt expression statement -> failure x
  IfStmntWithElse expression statement1 statement2 -> failure x

transPrintStatement :: PrintStatement -> VEnv -> IO VEnv
transPrintStatement x env = case x of
  PrintStmnt expression -> do
    (val, env') <- transExpression expression env
    print val
    return (env')

compareExpressions :: SimpleExpression -> SimpleExpression -> VEnv -> (Value -> Value -> Bool) -> IO (Value, VEnv)
compareExpressions simExp1 simExp2 env comparer = do
  (val1, env') <- transSimpleExpression simExp1 env
  (val2, env'') <- transSimpleExpression simExp2 env'
  return (VBool(comparer val1 val2), env'')

transExpression :: Expression -> VEnv -> IO (Value, VEnv)
transExpression x env = case x of
  ExpSimple simpleexpression -> transSimpleExpression simpleexpression env
  ExpEqual simpleexpression1 simpleexpression2 -> (compareExpressions simpleexpression1 simpleexpression2 env (==))
  ExpNotEqual simpleexpression1 simpleexpression2 -> (compareExpressions simpleexpression1 simpleexpression2 env (/=))
  ExpLess simpleexpression1 simpleexpression2 -> (compareExpressions simpleexpression1 simpleexpression2 env (<))
  ExpLessOrEqual simpleexpression1 simpleexpression2 -> (compareExpressions simpleexpression1 simpleexpression2 env (<=))
  ExpGreater simpleexpression1 simpleexpression2 -> (compareExpressions simpleexpression1 simpleexpression2 env (>))
  ExpGreaterOrEqual simpleexpression1 simpleexpression2 -> (compareExpressions simpleexpression1 simpleexpression2 env (>=))

transSimpleExpression :: SimpleExpression -> VEnv -> IO (Value, VEnv)
transSimpleExpression x env = case x of
  SimpleExpTerm term -> transTerm term env
  SimpleExpAdd simpleexpression term -> do
    (val1, env') <- transSimpleExpression simpleexpression env
    (val2, env'') <- transTerm term env'
    case (val1, val2) of
        (VInt x, VInt y) -> return (VInt(x + y), env'')
  SimpleExpSubst simpleexpression term -> do
    (val1, env') <- transSimpleExpression simpleexpression env
    (val2, env'') <- transTerm term env'
    case (val1, val2) of
        (VInt x, VInt y) -> return (VInt(x - y), env'')

transTerm :: Term -> VEnv -> IO (Value, VEnv)
transTerm x env = case x of
  TermFactor factor -> transFactor factor env
  TermMultiply term factor -> do
    (val1, env') <- transTerm term env
    (val2, env'') <- transFactor factor env'
    case (val1, val2) of
        (VInt x, VInt y) -> return (VInt(x * y), env'')
  TermDivide term factor -> do
    (val1, env') <- transTerm term env
    (val2, env'') <- transFactor factor env'
    case (val1, val2) of
        (VInt x, VInt y) -> if y /=0 then return (VInt(div x y), env'')
                            else error ("division by 0")

transFactor :: Factor -> VEnv -> IO (Value, VEnv)
transFactor x env = case x of
  FactorExpression expression -> transExpression expression env
  FactorPlus factor -> transFactor factor env
  FactorMinus factor -> do
    (val, env') <- transFactor factor env
    case val of
        VInt x -> return(VInt(-x), env')
  --FactorFunctionCall functioncall -> failure x
  FactorConstant constant -> return (transConstant constant, env)
  FactorIdent ident -> do
    return (getVarVal env ident, env)
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

