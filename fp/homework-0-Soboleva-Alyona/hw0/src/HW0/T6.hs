module HW0.T6(
    a, a_whnf,
    b, b_whnf,
    c, c_whnf
) where

import HW0.T1
import Data.Char

a = distrib (Left ("AB" ++ "CD" ++ "EF"))     -- distrib from HW0.T1
b = map isSpace "Hello, World"
c = if 1 > 0 || error "X" then "Y" else "Z"

a_whnf :: (Either [Char] a, Either [Char] b)
a_whnf = (Left ("AB" ++ "CD" ++ "EF"), Left ("AB" ++ "CD" ++ "EF"))

b_whnf :: [Bool]
b_whnf = False : map isSpace "ello, World" 

c_whnf :: [Char]
c_whnf = "Y"