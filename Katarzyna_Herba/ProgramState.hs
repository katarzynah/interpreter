module ProgramState
(ProgramState,
 emptyGEnv,
 declareVar,
 declareVars,
 setVarVal,
 setVarsVals,
 setArrayVal,
 getVarVal,
 getArrayVal,
 setDecl,
 getDecl,
 addLocalEnvironment,
 exitLocalEnvironment)
where

import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Sequence as Seq

import Value
import AbsInterpreter

type ProgramState = StateT GEnv IO

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

-- Returns empty environment.
emptyEnv :: Env
emptyEnv = (Map.empty, Map.empty)

-- Returns empty global environment.
emptyGEnv :: GEnv
emptyGEnv = [emptyEnv]

-- Given int and a value returns VArray ([value] * n).
_getArrayOfValues :: Int -> Value -> Value
_getArrayOfValues n val | n <= 0 = error ("Array dimensions must be " ++
                                          "positive integers.")
                        | otherwise = VArray (Seq.replicate n val)

-- Returns matrix of given dimenensions of VNull.
_getValueToDeclare :: [Int] -> Value
_getValueToDeclare [] = VNull
_getValueToDeclare (int : ints) =
  _getArrayOfValues int (_getValueToDeclare ints)

-- Add variable identifier to the most local environment with unspecified value.
declareVar :: Ident -> [Int] -> ProgramState ()
declareVar ident dims = state $ \(env) ->
  case env of
    ((localVEnv, localPEnv) : envs) ->
      case Map.lookup ident localVEnv of
        Nothing -> ((), (Map.insert ident value localVEnv, localPEnv) : envs) where
          value =  _getValueToDeclare dims
        Just _ -> error ("Variable " ++ show ident ++ " already declared.")

-- Add a list of variable identifiers to the most local environment with
-- unspecified value.
declareVars :: [Ident] -> [Int] -> ProgramState ()
declareVars [] _ = return ()
declareVars (ident : idents) dims = do
  declareVar ident dims
  declareVars idents dims

_getValueToFetch :: Value -> [Int] -> Value
_getValueToFetch val [] = val
_getValueToFetch val (dim : dims) = case val of
  VArray values -> 
    if dim >= Seq.length values then error "Index out of bounds." else
    _getValueToFetch (Seq.index values dim) dims
  _ -> error "Wrong number of dimensions provided!"

_getVarVal :: GEnv -> Ident -> Value
_getVarVal [] ident = error ("Variable " ++ show ident ++ " not defined.")
_getVarVal ((localVEnv, _) : envs) ident =
  case Map.lookup ident localVEnv of
    Nothing -> _getVarVal envs ident
    Just val -> val

-- Get value of variable with given identifier.
getVarVal :: Ident -> ProgramState Value
getVarVal ident = state $ \(env) -> ((_getVarVal env ident), env)

-- Get value of already declared array at given indices.
_getArrayVal :: GEnv -> Ident -> [Int] -> Value
_getArrayVal [] ident _ = error ("Variable " ++ show ident ++ " not defined.")
_getArrayVal ((localVEnv, _) : envs) ident dims =
  case Map.lookup ident localVEnv of
    Nothing -> _getArrayVal envs ident dims
    Just val -> _getValueToFetch val dims

-- Get value of already declared array at given indices.
getArrayVal :: Ident -> [Int] -> ProgramState Value
getArrayVal ident dims = state $ \(env) -> (_getArrayVal env ident dims, env)

-- Set value of already declared variable with given identifier.
_setVarVal :: GEnv -> Ident -> Value -> GEnv
_setVarVal [] ident val = error ("Variable " ++ show ident ++ " not defined.")
_setVarVal ((localVEnv, localPEnv) : envs) ident val =
  case Map.lookup ident localVEnv of
    Nothing -> ((localVEnv, localPEnv) : _setVarVal envs ident val)
    Just _ -> ((Map.insert ident val localVEnv, localPEnv) : envs)

-- Set value of already declared variable with given identifier.
setVarVal :: Ident -> Value -> ProgramState ()
setVarVal ident val = state $ \(env) -> ((), _setVarVal env ident val)

-- Set values of a list of idents. The lists must be of same length!
setVarsVals :: [Ident] -> [Value] -> ProgramState ()
setVarsVals [] _ = return ()
setVarsVals (ident : idents) (val : vals) = do
  setVarVal ident val
  setVarsVals idents vals

_setValueInArray :: Value -> [Int] -> Value -> Value
_setValueInArray array [] _ =
  error "You cannot assign to array, just its elements."
_setValueInArray array (x:[]) val =
  case array of
    VArray sequen ->
      if x >= Seq.length sequen then error "Index out of bounds." else
      VArray (Seq.update x val sequen)
    _ -> error "Wrong number of dimensions provided."
_setValueInArray array (x:xs) val =
  case array of
    VArray sequen -> 
      if x >= Seq.length sequen then error "Index out of bounds." else
      VArray (Seq.update x newValue sequen) where
        newValue =_setValueInArray (Seq.index sequen x) xs val
    _ -> error "Wrong number of dimensions provided."

_setArrayVal :: GEnv -> Ident -> [Int] -> Value -> GEnv
_setArrayVal [] ident _ _ = error ("Variable " ++ show ident ++ " not defined.")
_setArrayVal ((localVEnv, localPEnv) : envs) ident dims val =
  case Map.lookup ident localVEnv of
    Nothing -> ((localVEnv, localPEnv) : _setArrayVal envs ident val)
    Just array@(VArray _) ->
      ((Map.insert ident (_setValueInArray array dims val) localVEnv,
        localPEnv) : envs)
    Just _ -> error ("Variable " ++ show ident ++ " is not an array.")

-- Set value of already declared array at given indices.
setArrayVal :: Ident -> [Int] -> Value -> ProgramState ()
setArrayVal ident dims val = state $ \(env) ->
  ((), _setArrayVal env ident dims val)

-- Add function/procedure identifier to the most local environment.
setDecl :: Ident -> ProcDec -> ProgramState ()
setDecl ident procDec = state $ \(env) ->
  case env of
    ((localVEnv, localPEnv) : envs) ->
      case Map.lookup ident localPEnv of
        Nothing -> ((), (localVEnv, Map.insert ident procDec localPEnv) : envs)
        Just _ -> error ("Function/procedure " ++ show ident ++
                         " already defined.")

-- Get definition of function/procedure with given identifier.
_getDecl :: GEnv -> Ident -> ProcDec
_getDecl [] ident = error ("Function/procedure " ++ show ident ++
                          " not defined.")
_getDecl ((_, localPEnv) : envs) ident =
  case Map.lookup ident localPEnv of
    Nothing -> _getDecl envs ident
    Just val -> val

-- Get definition of function/procedure with given identifier.
getDecl :: Ident -> ProgramState ProcDec
getDecl ident = state $ \(env) -> (_getDecl env ident, env)

addLocalEnvironment :: ProgramState ()
addLocalEnvironment = state $ \(env) -> ((), (emptyEnv : env))

exitLocalEnvironment :: ProgramState ()
exitLocalEnvironment = state $ \(env) ->
  case env of
    (_ : globalEnv) -> ((), globalEnv)