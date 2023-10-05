module HW2.T4(
    State(..),
    mapState,
    wrapState,
    joinState,
    modifyState,
    Prim(..),
    Expr(..),
    eval
) where 

import Control.Monad
import HW2.T1

-- runS :: State s a -> s -> Annotated s a
data State s a = S { runS :: s -> Annotated s a }

mapState :: (a -> b) -> State s a -> State s b
mapState f st = S (\s -> mapAnnotated f (runS st s))

wrapState :: a -> State s a
wrapState a = S (\s -> a :# s)

joinState :: State s (State s a) -> State s a
joinState state = S (\s -> 
    let a :# sNew = runS state s 
    in runS a sNew)

modifyState :: (s -> s) -> State s ()
modifyState fs = S (\s -> (() :# (fs s)))

instance Functor (State s) where
  fmap = mapState

instance Applicative (State s) where
  pure = wrapState
  p <*> q = Control.Monad.ap p q

instance Monad (State s) where
   m >>= f = joinState (fmap f m)

data Prim a =
    Add a a      -- (+)
  | Sub a a      -- (-)
  | Mul a a      -- (*)
  | Div a a      -- (/)
  | Abs a        -- abs
  | Sgn a        -- signum
    --deriving (Show)

data Expr = Val Double | Op (Prim Expr) --deriving (Show)

instance Num Expr where
  x + y = Op (Add x y)
  x * y = Op (Mul x y)
  x - y = Op (Sub x y)
  abs x = Op (Abs x)
  signum x = Op (Sgn x)
  fromInteger x = Val (fromInteger x)

instance Fractional Expr where
  fromRational x = Val (fromRational x)
  x / y = Op (Div x y)

evalUnary :: Expr
  -> (Double -> Prim Double)
  -> (Double -> Double)
  -> State [Prim Double] Double
evalUnary x primOp numExprOp = do 
  resX <- eval x
  fmap (\_ -> numExprOp resX) (modifyState (primOp resX :))

evalBinary :: Expr 
  -> Expr
  -> (Double -> Double -> Prim Double)
  -> (Double -> Double -> Double)
  -> State [Prim Double] Double
evalBinary a b primOp numExprOp = do 
  resA <- eval a
  resB <- eval b
  fmap (\_ -> numExprOp resA resB) (modifyState (primOp resA resB :))

eval :: Expr -> State [Prim Double] Double
eval (Val a) = pure a 
eval (Op (Abs x)) = evalUnary x Abs abs
eval (Op (Sgn x)) = evalUnary x Sgn signum
eval (Op (Add a b)) = evalBinary a b Add (+)
eval (Op (Sub a b)) = evalBinary a b Sub (-)
eval (Op (Mul a b)) = evalBinary a b Mul (*)
eval (Op (Div a b)) = evalBinary a b Div (/)

