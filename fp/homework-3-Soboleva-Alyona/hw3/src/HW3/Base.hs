module HW3.Base (
    HiFun(..),
    HiValue(..),
    HiExpr(..),
    HiError(..)
) where

import Data.Ratio 
    ( Rational  )
import Data.Text 
    ( Text ) 

data HiFun =    -- function names (e.g. div, sort, length, ...)
  HiFunDiv
  | HiFunMul
  | HiFunAdd
  | HiFunSub
  -- 2 task
  | HiFunNot
  | HiFunAnd
  | HiFunOr
  | HiFunLessThan
  | HiFunGreaterThan
  | HiFunEquals
  | HiFunNotLessThan
  | HiFunNotGreaterThan
  | HiFunNotEquals
  | HiFunIf
  -- 3 task
  | HiFunLength
  | HiFunToUpper
  | HiFunToLower
  | HiFunReverse
  | HiFunTrim
  deriving (Ord, Eq)

data HiValue =  -- values (numbers, booleans, strings, ...)
  HiValueNumber Rational
  | HiValueFunction HiFun
  -- 2 task 
  | HiValueBool Bool 
  -- 3 task
  | HiValueNull
  | HiValueString Text
  deriving (Ord, Eq)

data HiExpr =  -- expressions (literals, function calls, ...)
  HiExprValue HiValue
  | HiExprApply HiExpr [HiExpr]

data HiError =   -- evaluation errors (invalid arguments, ...)
  HiErrorInvalidArgument
  | HiErrorInvalidFunction
  | HiErrorArityMismatch
  | HiErrorDivideByZero deriving (Show)
