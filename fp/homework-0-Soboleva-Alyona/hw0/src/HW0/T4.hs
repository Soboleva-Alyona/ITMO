module HW0.T4 (
    repeat',
    map',
    fac,
    fib
) where 

import Data.Function
import GHC.Natural

repeat' :: a -> [a]             -- behaves like Data.List.repeat
repeat' x = fix (x:) 

map' :: (a -> b) -> [a] -> [b]  -- behaves like Data.List.map
map' f a = fix mapHelp f a

mapHelp :: ((a -> b) -> [a] -> [b]) -> (a -> b) -> [a] -> [b]
mapHelp _ _ [] = []
mapHelp f g (h:t) = (g h: f g t)

fib :: Natural -> Natural       -- computes the n-th Fibonacci number
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fac :: Natural -> Natural       -- computes the factorial
fac 0 = 0
fac 1 = 1
fac n =  n * fac (n - 1) 