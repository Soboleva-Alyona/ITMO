module HW0.T2(
    Not(..),
    doubleNeg,
    reduceTripleNeg
)
 where

import Data.Void

type Not a = a -> Void

-- a -> (a -> Void) -> Void
doubleNeg :: a -> Not (Not a)
doubleNeg a x = x a
-- a -> Void -> a == Not a 

-- a -> Void -> Void -> Void -> a -> Void
reduceTripleNeg :: Not (Not (Not a)) -> Not a
reduceTripleNeg x a = x (doubleNeg a)