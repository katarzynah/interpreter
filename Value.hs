module Value
(Value (..),
 evaluateToInts)
where

import qualified Data.Sequence as Seq

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

-- Given a list of values, attempts to extract their int values to a list.
evaluateToInts :: [Value] -> [Int]
evaluateToInts [] = []
evaluateToInts (val : vals) = case val of
  VInt int -> if int >= 0 then (int : evaluateToInts vals)
              else error ("Arrays are indexed by nonzero ints.")
  _ -> error ("Arrays are indexed only by ints.")