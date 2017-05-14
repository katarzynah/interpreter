module ProgramState
where

import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Sequence as Seq

import Environment
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

-- ((localVEnv, localPEnv) : envs)
declareVar :: Ident -> [Int] -> ProgramState ()
declareVar ident dims = state $ \(env) ->
  case env of
    ((localVEnv, localPEnv) : envs) ->
      case Map.lookup ident localVEnv of
        Nothing -> ((), (Map.insert ident value localVEnv, localPEnv) : envs) where
          value =  _getValueToDeclare dims
        Just _ -> error ("Variable " ++ show ident ++ " already declared.")

declareVars :: [Ident] -> [Int] -> ProgramState ()
declareVars [] _ = return ()
declareVars (ident : idents) dims = do
  declareVar ident dims
  declareVars idents dims

_getVarVal :: GEnv -> Ident -> Value
_getVarVal [] ident = error ("Variable " ++ show ident ++ " not defined.")
_getVarVal ((localVEnv, _) : envs) ident =
  case Map.lookup ident localVEnv of
    Nothing -> _getVarVal envs ident
    Just val -> val

-- Get value of variable with given identifier.
getVarVal :: Ident -> ProgramState Value
getVarVal ident = state $ \(env) -> ((_getVarVal env ident), env)

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

-- Add function/procedure identifier to the most local environment.
setDecl :: Ident -> ProcDec -> ProgramState ()
setDecl ident procDec = state $ \(env) ->
  case env of
    ((localVEnv, localPEnv) : envs) ->
      case Map.lookup ident localPEnv of
        Nothing -> ((), (localVEnv, Map.insert ident procDec localPEnv) : envs)
        Just _ -> error ("Function/procedure " ++ show ident ++
                         " already defined.")