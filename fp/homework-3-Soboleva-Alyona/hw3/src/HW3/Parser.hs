{-# LANGUAGE OverloadedStrings #-}

module HW3.Parser(
  parse
) where 

import HW3.Base
import Text.Megaparsec hiding (parse)
import Data.Void
import Data.Text (Text, pack)
import Text.Megaparsec.Char (string, space1, char)
import qualified Text.Megaparsec.Char.Lexer as L 
import Control.Applicative (empty)
import Control.Monad.Combinators.Expr
import Control.Monad (void)
import Data.Char (isAlphaNum, isAlpha)
import HW3.Base (HiFun(HiFunNot, HiFunLessThan, HiFunEquals, HiFunDiv), HiValue (HiValueBool, HiValueNull))

type Parser = Parsec Void String

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

sc :: Parser ()
sc = L.space
  space1
  empty       -- line comments not available
  empty       -- block comments not available

parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse = runParser (pExpr <* eof) ""

pExpr :: Parser HiExpr
pExpr = makeExprParser (sc *> parseExpr <* sc) operatorTable

parseExpr :: Parser HiExpr
parseExpr =
   do
    val  <- parseHiExprValue      <* sc
    args <- many $ parseArguments <* sc
    let expr = if null args then val else foldl HiExprApply val args
    return expr

parseHiExprValue :: Parser HiExpr
parseHiExprValue = HiExprValue <$> choice
  [ parseHiValueNumber
  , parseHiValueFunction
  , parseHiValueBool
  , parseHiValueString
  , parseHiValueNull
  ] <|> parens pExpr

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

parseArguments :: Parser [HiExpr]
parseArguments =
  do
    void $ string "(" <* sc
    args <- (pExpr  <* sc) `sepBy` (string ","  <* sc)
    void $ string ")"
    return args
    
parseHiValueFunction :: Parser HiValue
parseHiValueFunction = HiValueFunction <$> lexeme (
  choice [
    HiFunDiv <$ string "div",
    HiFunMul <$ string "mul",
    HiFunAdd <$ string "add",
    HiFunSub <$ string "sub",
    
    HiFunNotEquals <$ "not-equals",
    HiFunNotLessThan <$ "not-less-than",
    HiFunNotGreaterThan <$ "not-greater-than",
    HiFunNot <$ string "not",
    HiFunAnd <$ string "and",
    HiFunOr <$ string "or",
    HiFunLessThan <$ "less-than",
    HiFunGreaterThan <$ "greater-than",
    HiFunEquals <$ "equals",
    HiFunIf <$ "if",

    HiFunLength <$ "length",
    HiFunToUpper <$ "to-upper",
    HiFunToLower <$ "to-lower",
    HiFunReverse <$ "reverse",
    HiFunTrim <$ "trim"
  ]
  )

parseHiValueNumber :: Parser HiValue
parseHiValueNumber = HiValueNumber <$> rational

parseHiValueBool :: Parser HiValue 
parseHiValueBool = HiValueBool <$> boolean

parseHiValueNull :: Parser HiValue
parseHiValueNull = HiValueNull <$ "null"

parseHiValueString :: Parser HiValue
parseHiValueString = do
  operation <- Text.Megaparsec.Char.char '"' *> (pack <$> Text.Megaparsec.manyTill L.charLiteral (char '"')) <* sc
  return $ HiValueString operation

rational :: Parser Rational
rational = lexeme (L.signed sc L.scientific) >>= return . toRational

boolean :: Parser Bool 
boolean = choice
   [ True <$ string "true"
   , False <$ string "false"
   ]

operatorTable :: [[Operator Parser HiExpr]]
operatorTable =
  [ [ binaryLeft "*" HiFunMul
    , binaryLeftDiv
    ], 
    [ binaryLeft "+" HiFunAdd
    , binaryLeft "-" HiFunSub
    ],
    [ binaryNon ">=" HiFunNotLessThan
    , binaryNon "<=" HiFunNotGreaterThan
    , binaryNon "==" HiFunEquals
    , binaryNon "not" HiFunNot
    , binaryNon "<" HiFunLessThan
    , binaryNon ">" HiFunGreaterThan
    , binaryNon "/=" HiFunNotEquals
    , binaryNon "if" HiFunIf
    ],
    [ binaryRight "&&" HiFunAnd
    , binaryRight "||" HiFunOr
    ]
  ]
  where
  
  binaryLeft :: String -> HiFun -> Operator Parser HiExpr
  binaryLeft name function = InfixL $ infixBinary function <$ symbol name

  binaryLeftDiv :: Operator Parser HiExpr
  binaryLeftDiv = InfixL (infixBinary HiFunDiv <$ try (symbol "/" <* notFollowedBy (char '=')))

  binaryNon :: String -> HiFun -> Operator Parser HiExpr
  binaryNon name function = InfixN $ infixBinary function <$ symbol name

  binaryRight :: String -> HiFun -> Operator Parser HiExpr
  binaryRight name function = InfixR $ infixBinary function <$ symbol name

  infixBinary:: HiFun -> HiExpr -> HiExpr -> HiExpr
  infixBinary function a b = HiExprApply (HiExprValue (HiValueFunction function)) [a, b]



