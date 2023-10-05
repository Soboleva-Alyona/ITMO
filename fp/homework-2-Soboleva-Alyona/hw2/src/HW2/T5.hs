module HW2.T5(
    ExceptState(..),
    mapExceptState,
    wrapExceptState,
    joinExceptState,
    modifyExceptState,
    throwExceptState,
    EvaluationError(..),
    eval
) where 

import Control.Monad
import HW2.T1
import HW2.T4 hiding (eval)

-- ExceptState e s a -> s -> Except e (Annotated s a)
data ExceptState e s a = ES { runES :: s -> Except e (Annotated s a) }

mapExceptState :: (a -> b) -> ExceptState e s a -> ExceptState e s b
mapExceptState f st = ES $
    \s -> case runES st s of
        Error e -> Error e 
        Success a -> Success (mapAnnotated f a)

wrapExceptState :: a -> ExceptState e s a
wrapExceptState a = ES (\s -> Success (a :# s))

joinExceptState :: ExceptState e s (ExceptState e s a) -> ExceptState e s a
joinExceptState st = ES $
    \s -> case runES st s of
        Error e -> Error e 
        Success (a :# e1) -> runES a e1

modifyExceptState :: (s -> s) -> ExceptState e s ()
modifyExceptState f = ES (\s -> Success (() :# f s))

throwExceptState :: e -> ExceptState e s a
throwExceptState e = ES (\_ -> Error e)

instance Functor (ExceptState e s) where
    fmap = mapExceptState

instance Applicative (ExceptState e s) where
    pure = wrapExceptState
    p <*> q = Control.Monad.ap p q

instance Monad (ExceptState e s) where
    m >>= f = joinExceptState (fmap f m)


data EvaluationError = DivideByZero

evalUnary :: Expr
    -> (Double -> Prim Double)
    -> (Double -> Double)
    -> ExceptState EvaluationError [Prim Double] Double
evalUnary x primOp numExprOp = do 
    resX <- eval x
    fmap (\_ -> numExprOp resX) (modifyExceptState (primOp resX :))

evalBinary :: Expr 
    -> Expr
    -> (Double -> Double -> Prim Double)
    -> (Double -> Double -> Double)
    -> ExceptState EvaluationError [Prim Double] Double
evalBinary a b primOp numExprOp = do 
    resA <- eval a
    resB <- eval b
    fmap (\_ -> numExprOp resA resB) (modifyExceptState (primOp resA resB :))

eval :: Expr -> ExceptState EvaluationError [Prim Double] Double
eval (Val a) = pure a 
eval (Op (Abs x)) = evalUnary x Abs abs
eval (Op (Sgn x)) = evalUnary x Sgn signum
eval (Op (Add a b)) = evalBinary a b Add (+)
eval (Op (Sub a b)) = evalBinary a b Sub (-)
eval (Op (Mul a b)) = evalBinary a b Mul (*)
eval (Op (Div a b)) = do 
    resA <- eval a
    resB <- eval b
    if (resB == 0.0)
        then throwExceptState DivideByZero
        else fmap (\_ -> resA / resB) (modifyExceptState (Div resA resB :))