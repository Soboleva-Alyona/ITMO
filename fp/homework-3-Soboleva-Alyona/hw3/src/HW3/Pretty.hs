module HW3.Pretty(
  prettyValue
) where

import Prettyprinter
import Prettyprinter.Render.Terminal.Internal
import Data.Scientific 
    ( fromRationalRepetendUnlimited, 
      FPFormat(..), 
      formatScientific, 
      Scientific)
import Data.Ratio 
    ( denominator, 
      numerator)
import HW3.Base

prettyValue :: HiValue -> Doc AnsiStyle
prettyValue (HiValueNumber n) = getPrettyNumber n (fromRationalRepetendUnlimited n)
prettyValue (HiValueBool b) = pretty b
prettyValue (HiValueFunction f) = getPrettyFunction f
prettyValue HiValueNull = pretty "null"
prettyValue (HiValueString txt) = pretty txt

getPrettyNumber :: Rational -> (Scientific, Maybe Int) -> Doc AnsiStyle
getPrettyNumber rational (sc, Nothing) = 
  case denominator rational of
    1 -> pretty (numerator rational)
    _ -> pretty (formatScientific Fixed Nothing sc)
getPrettyNumber rational (_, Just _) = do
  let num = numerator rational
  let den = denominator rational
  let (integerPart, rem) = quotRem num den
  let prettyFraction = pretty (abs num) <> pretty "/" <> pretty den
  let prettyIntegerPart = pretty (quot num den)
  let prettyFractionOnly = pretty rem <> pretty "/" <> pretty den
  case integerPart of 
    0 -> getFractional num prettyFraction
    _ -> getMixedFractional num prettyIntegerPart prettyFractionOnly
    where 
      getFractional num prettyFraction = 
        if num > 0
        then prettyFraction
        else pretty "-" <> prettyFraction
      getMixedFractional num prettyIntegerPart prettyFractionOnly = 
        if num > 0
        then prettyIntegerPart <+> pretty "+" <+> prettyFractionOnly
        else pretty "-" <> prettyIntegerPart <+> pretty "-" <+> prettyFractionOnly

getPrettyFunction HiFunDiv = pretty "div"
getPrettyFunction HiFunAdd = pretty "add"
getPrettyFunction HiFunMul = pretty "mul"
getPrettyFunction HiFunSub = pretty "sub"
getPrettyFunction HiFunNot = pretty "not"
getPrettyFunction HiFunAnd = pretty "and"
getPrettyFunction HiFunOr = pretty "or"
getPrettyFunction HiFunLessThan = pretty "less-than"
getPrettyFunction HiFunGreaterThan = pretty "greater-than"
getPrettyFunction HiFunEquals = pretty "equals"
getPrettyFunction HiFunNotLessThan = pretty "not-less-than"
getPrettyFunction HiFunNotGreaterThan = pretty "not-greater-than"
getPrettyFunction HiFunNotEquals = pretty "equals"
getPrettyFunction HiFunIf = pretty "if"
