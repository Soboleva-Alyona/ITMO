module HW1.T2(
    N(..),
    nplus,
    nmult,
    nsub, 
    ncmp,
    nFromNatural,
    nToNum, 
    nEven, 
    nOdd,
    nmod,
    ndiv
) where 

import Numeric.Natural

data N = Z | S N deriving (Show)

nplus :: N -> N -> N        -- addition
nplus n Z = n  
nplus Z n = n 
nplus (S n1) (S n2) = S (S (nplus n1 n2))

nmult :: N -> N -> N        -- multiplication
nmult _ Z = Z  
nmult Z _ = Z
nmult n (S Z) = n
nmult (S Z) n = n
nmult (S n1) (S n2) = nplus (S n1) (nmult (S n1) n2)

nsub :: N -> N -> Maybe N   -- subtraction     (Nothing if result is negative)
nsub Z Z = Just Z
nsub Z _ = Nothing
nsub n Z = Just n 
nsub (S n1) (S n2) = nsub n1 n2

ncmp :: N -> N -> Ordering  -- comparison      (Do not derive Ord)
ncmp Z Z = EQ 
ncmp _ Z = GT
ncmp Z _ = LT
ncmp (S n1) (S n2) = ncmp n1 n2


nFromNatural :: Natural -> N
nFromNatural 0 = Z
nFromNatural n = S (nFromNatural (n - 1))

nToNum :: Num a => N -> a
nToNum Z = 0
nToNum (S n) = nToNum n + 1

nEven, nOdd :: N -> Bool    -- parity checking
nEven Z = True
nEven (S n) = nOdd n 

nOdd Z = False
nOdd (S n) = nEven n 

helpSub :: N -> N -> N   -- subtraction     (Nothing if result is negative)
helpSub Z Z = Z
helpSub Z _ = error "only natural digits"
helpSub n Z = n
helpSub (S n1) (S n2) =  helpSub n1 n2

ndiv :: N -> N -> N         -- integer division
ndiv _ Z = error "can't divide by zero"
ndiv Z _ = Z
ndiv n (S Z) = n 
ndiv n (S d)  
   | ncmp n (S d) == LT = Z
   | ncmp n (S d) == EQ = S Z
   | otherwise = S (ndiv (helpSub n (S d)) (S d))
                 
nmod :: N -> N -> N         -- modulo operation
nmod _ Z = error "can't divide by zero"
nmod Z _ = Z
nmod (S n) d = helpSub (S n) (nmult (ndiv (S n) d) d)
