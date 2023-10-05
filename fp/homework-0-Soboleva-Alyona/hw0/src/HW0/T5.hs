module HW0.T5 (
    Nat,
    nz, 
    ns,
    nplus,
    nmult,
    nFromNatural,
    nToNum
) where

import GHC.Natural

type Nat a = (a -> a) -> a -> a

nz :: Nat a
nz _ a = a 

ns :: Nat a -> Nat a
ns fun f a = f (fun f a)  

nplus, nmult :: Nat a -> Nat a -> Nat a
nplus a b f x =  b f $ a f x

nmult a b f = a $ b f


nFromNatural :: Natural -> Nat a
nFromNatural 0 = nz 
nFromNatural n = ns $ nFromNatural $ n - 1

nToNum :: Num a => Nat a -> a
nToNum n = n (\x -> x + 1) 0
