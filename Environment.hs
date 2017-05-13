module Environment
(GEnv,
 Env,
 Value (..),
 emptyGEnv,
 emptyEnv,
 declareVar,
 declareVars,
 setVarVal,
 setVarsVals,
 setArrayVal,
 getVarVal,
 getArrayVal,
 setDecl,
 getDecl)
where

import AbsInterpreter

import qualified Data.Map as Map
import qualified Data.Sequence as Seq

-- Type mapping variable identificators to their values.
type VEnv = Map.Map Ident Value

-- Type mapping function/procedure identificators to their declarations.
type PEnv = Map.Map Ident ProcDec

-- Type containing both of the above.
type Env = (VEnv, PEnv)

-- Type containing list of environments - each next index is the next embedded
-- level of declarations. It keeps the levels in revers order, so that the most
-- global declarations are at the end of the list.
type GEnv = [Env]

-- Type for storing values that are used in the program (what variables can
-- evaluate to).
data Value = VInt Int |
             VBool Bool |
             VStr String |
             VArray (Seq.Seq Value) |
             VNull

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

-- Returns empty environment.
emptyEnv :: Env
emptyEnv = (Map.empty, Map.empty)

-- Returns empty global environment.
emptyGEnv :: GEnv
emptyGEnv = [emptyEnv]

-- Given int and a value returns VArray ([value] * n).
_getArrayOfValues :: Int -> Value -> Value
_getArrayOfValues n val | n <= 0 = error ("Array dimensions must be " ++
                                          "positive integers")
                        | otherwise = VArray (Seq.replicate n val)

-- Returns matrix of given dimenensions of VNull.
_getValueToDeclare :: [Int] -> Value
_getValueToDeclare [] = VNull
_getValueToDeclare (int : ints) =
  _getArrayOfValues int (_getValueToDeclare ints)

-- Add variable identifier to the most local environment with unspecified value.
declareVar :: GEnv -> Ident -> [Int] -> GEnv
declareVar ((localVEnv, localPEnv) : envs) ident dims =
  case Map.lookup ident localVEnv of
    Nothing -> ((Map.insert ident value localVEnv, localPEnv) : envs) where
      value = _getValueToDeclare dims
    Just _ -> error("Variable " ++ show ident ++ " already declared")

-- Add a list of variable identifiers to the most local environment with
-- unspecified value.
declareVars :: GEnv -> [Ident] -> [Int] -> GEnv
declareVars env [] _ = env
declareVars env (ident : idents) dims =
  declareVars (declareVar env ident dims) idents dims

-- Set value of already declared variable with given identifier.
setVarVal :: GEnv -> Ident -> Value -> GEnv
setVarVal [] ident val = error ("Variable " ++ show ident ++ " not defined")
setVarVal ((localVEnv, localPEnv) : envs) ident val =
  case Map.lookup ident localVEnv of
    Nothing -> ((localVEnv, localPEnv) : setVarVal envs ident val)
    Just _ -> ((Map.insert ident val localVEnv, localPEnv) : envs)

-- Set values of a list of idents. The lists must be of same length!
setVarsVals :: GEnv -> [Ident] -> [Value] -> GEnv
setVarsVals env [] _ = env
setVarsVals env (ident : idents) (val : vals) =
  setVarsVals (setVarVal env ident val) idents vals

_setValueInArray :: Value -> [Int] -> Value -> Value
_setValueInArray array [] _ =
  error "You cannot assign to array, just it's elements"
_setValueInArray array (x:[]) val =
  case array of
    VArray sequen ->
      if x >= Seq.length sequen then error ("Index out of bounds") else
      VArray (Seq.update x val sequen)
    _ -> error "Wrong number of dimensions provided"
_setValueInArray array (x:xs) val =
  case array of
    VArray sequen -> 
      if x >= Seq.length sequen then error ("Index out of bounds") else
      VArray (Seq.update x newValue sequen) where
        newValue =_setValueInArray (Seq.index sequen x) xs val
    _ -> error "Wrong number of dimensions provided"

-- Set value of already declared array at given indices.
setArrayVal :: GEnv -> Ident -> [Int] -> Value -> GEnv
setArrayVal [] ident _ _ = error ("Variable " ++ show ident ++ " not defined")
setArrayVal ((localVEnv, localPEnv) : envs) ident dims val =
  case Map.lookup ident localVEnv of
    Nothing -> ((localVEnv, localPEnv) : setVarVal envs ident val)
    Just array@(VArray _) ->
      ((Map.insert ident (_setValueInArray array dims val) localVEnv,
        localPEnv) : envs)
    Just _ -> error ("Variable " ++ show ident ++ " is not an array")

_getValueToFetch :: Value -> [Int] -> Value
_getValueToFetch val [] = val
_getValueToFetch val (dim : dims) = case val of
  VArray values -> 
    if dim >= Seq.length values then error ("Index out of bounds") else
    _getValueToFetch (Seq.index values dim) dims
  _ -> error ("Wrong number of dimensions provided!")

-- Get value of variable with given identifier.
getVarVal :: GEnv -> Ident -> Value
getVarVal [] ident = error("Variable " ++ show ident ++ " not defined")
getVarVal ((localVEnv, _) : envs) ident =
  case Map.lookup ident localVEnv of
    Nothing -> getVarVal envs ident
    Just val -> val

-- Set value of already declared array at given indices.
getArrayVal :: GEnv -> Ident -> [Int] -> Value
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