module HW3.Evaluator(
  eval
) where

import HW3.Base
    ( HiError(..),
      HiExpr(..),
      HiValue(..),
      HiFun(..) )
import Control.Monad.Trans.Except 
    ( ExceptT, 
      runExceptT, 
      ExceptT(..), 
      throwE )
import Data.Text as T
    ( length,
      toUpper,
      toLower,
      reverse,
      strip, 
      append,
      pack,
      index )
import Data.Semigroup
    ( stimes )
import Data.Ratio  
    ( denominator, numerator )

eval :: Monad m => HiExpr -> m (Either HiError HiValue)
eval (HiExprValue value) = return $ Right value
eval (HiExprApply expr args) = runExceptT $ checkFun expr args

checkFun :: Monad m => HiExpr -> [HiExpr] -> ExceptT HiError m HiValue
checkFun expr args = do
  op <- ExceptT (eval expr)
  checkHiValueFunction op args

checkHiValueFunction :: Monad m => HiValue -> [HiExpr] -> ExceptT HiError m HiValue
checkHiValueFunction (HiValueFunction HiFunIf) args = executeIf args
checkHiValueFunction (HiValueFunction HiFunAnd) args = executeAnd args
checkHiValueFunction (HiValueFunction HiFunOr) args = executeOr args
checkHiValueFunction x@(HiValueString _) args = do
  convertedArgs <- getArgs args
  getSliceOrOndex x convertedArgs
checkHiValueFunction (HiValueFunction fun) args = do
  convertedArgs <- getArgs args
  executeHiValueFun fun convertedArgs
checkHiValueFunction _ _ = throwE HiErrorInvalidFunction

getSliceOrOndex :: Monad m => HiValue -> [HiValue] -> ExceptT HiError m HiValue
getSliceOrOndex t args = case Prelude.length args of
  1 -> getIndex t (head args)
  _ -> throwE HiErrorArityMismatch

getIndex :: Monad m => HiValue -> HiValue -> ExceptT HiError m HiValue
getIndex (HiValueString s) (HiValueNumber i)
  | not (isInteger i) = throwE HiErrorInvalidArgument
  | not (0 <= idx && idx < T.length s) = return HiValueNull
  | otherwise = return $ HiValueString (T.pack [T.index s idx])
  where idx = round i

isInteger :: Rational -> Bool
isInteger x = x == fromInteger (round x)

executeIf :: Monad m => [HiExpr] -> ExceptT HiError m HiValue
executeIf [cond, a, b] = do 
  conditionRes <- ExceptT $ eval cond 
  case conditionRes of 
    HiValueBool True -> ExceptT $ eval a 
    HiValueBool False -> ExceptT $ eval b
    _ -> throwE HiErrorInvalidArgument
executeIf _ = throwE HiErrorInvalidFunction

executeAnd :: Monad m => [HiExpr] -> ExceptT HiError m HiValue
executeAnd [a, b] = do 
  res1 <- ExceptT $ eval a 
  case res1 of 
    HiValueBool False -> return res1
    HiValueBool True -> do
      res2 <- ExceptT $ eval b 
      case res2 of 
        HiValueBool _ -> return res2 
        _ -> throwE HiErrorInvalidFunction
    _ -> throwE HiErrorInvalidArgument
executeAnd _ = throwE HiErrorInvalidFunction

executeOr :: Monad m => [HiExpr] -> ExceptT HiError m HiValue
executeOr [a, b] = do 
  res1 <- ExceptT $ eval a 
  case res1 of 
    HiValueBool True -> return res1
    HiValueBool False -> do
      res2 <- ExceptT $ eval b 
      case res2 of 
        HiValueBool _ -> return res2 
        _ -> throwE HiErrorInvalidFunction
    _ -> throwE HiErrorInvalidArgument
executeOr _ = throwE HiErrorInvalidFunction

executeHiValueFun :: Monad m => HiFun -> [HiValue] -> ExceptT HiError m HiValue
-- arithmetics
executeHiValueFun HiFunAdd [HiValueNumber n1, HiValueNumber n2] = return $ HiValueNumber $ n1 + n2
executeHiValueFun HiFunSub [HiValueNumber n1, HiValueNumber n2] = return $ HiValueNumber $ n1 - n2
executeHiValueFun HiFunMul [HiValueNumber n1, HiValueNumber n2] = return $ HiValueNumber $ n1 * n2
executeHiValueFun HiFunDiv [HiValueNumber n1, HiValueNumber n2] = 
  if n2 == 0 
  then throwE  HiErrorDivideByZero
  else return $ HiValueNumber $ n1 / n2
-- boolean 
executeHiValueFun HiFunNot [HiValueBool b] = return $ HiValueBool $ not b
executeHiValueFun HiFunLessThan [a, b] = return $ HiValueBool $ (<) a b
executeHiValueFun HiFunGreaterThan [a, b] = return $ HiValueBool $ (>) a b
executeHiValueFun HiFunEquals [a, b] = return $ HiValueBool $ a == b
executeHiValueFun HiFunNotLessThan [a, b] = return $ HiValueBool $ (>=) a b
executeHiValueFun HiFunNotGreaterThan [a, b] = return $ HiValueBool $ (<=) a b 
executeHiValueFun HiFunNotEquals [a, b] = return $ HiValueBool $ (/=) a b
-- strings
executeHiValueFun HiFunLength [HiValueString text] = return $ HiValueNumber $ toRational $ T.length text
executeHiValueFun HiFunToUpper [HiValueString text] = return $ HiValueString $ toUpper text
executeHiValueFun HiFunToLower [HiValueString text] = return $ HiValueString $ toLower text
executeHiValueFun HiFunReverse [HiValueString text] = return $ HiValueString $ T.reverse text
executeHiValueFun HiFunTrim [HiValueString text] = return $ HiValueString $ strip text
executeHiValueFun HiFunAdd [HiValueString t1, HiValueString t2] = return $ HiValueString $ append t1 t2
executeHiValueFun HiFunMul [HiValueString t, HiValueNumber n] = do 
  cnt <- getIfPositive n 
  return $ HiValueString $ stimes cnt t
executeHiValueFun HiFunDiv [HiValueString t1, HiValueString t2] = return $ HiValueString $ t1 <> T.pack "/" <> t2

executeHiValueFun function args =
  if argsNumber function == Prelude.length args
  then throwE HiErrorInvalidArgument
  else throwE HiErrorArityMismatch

getArgs :: Monad m => [HiExpr] -> ExceptT HiError m [HiValue]
getArgs [] = return []
getArgs (x : xs) = do
  first <- ExceptT $ eval x
  rest <- getArgs xs 
  return (first : rest)
  
argsNumber :: HiFun -> Int
argsNumber HiFunAdd = 2
argsNumber HiFunSub = 2
argsNumber HiFunMul = 2
argsNumber HiFunDiv = 2
argsNumber HiFunNot = 1
argsNumber HiFunAnd = 2
argsNumber HiFunOr = 2
argsNumber HiFunLessThan = 2
argsNumber HiFunGreaterThan = 2
argsNumber HiFunEquals = 2
argsNumber HiFunNotEquals = 2
argsNumber HiFunNotLessThan = 2
argsNumber HiFunNotGreaterThan = 2
argsNumber HiFunIf = 3
argsNumber HiFunLength = 1
argsNumber HiFunToUpper = 1
argsNumber HiFunToLower = 1
argsNumber HiFunReverse = 1
argsNumber HiFunTrim = 1

getIfPositive :: Monad m => Rational ->  ExceptT HiError m Integer
getIfPositive n = 
  if denominator n == 1
  then return $ numerator n
  else throwE HiErrorInvalidArgument

