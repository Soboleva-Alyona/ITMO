{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HW2.T6(
    ParseError (..), 
    Parser (..),
    pChar,
    pEof, 
    parseError, 
    parseExpr, 
    runP
) where

import HW2.T1 hiding(P)
import HW2.T5
import HW2.T4
import GHC.Natural (Natural)
import Control.Applicative (Alternative (..))
import Control.Applicative (optional)
import Control.Monad (MonadPlus, mfilter, void)

data ParseError = ErrorAtPos Natural

newtype Parser a = P (ExceptState ParseError (Natural, String) a)
  deriving newtype (Functor, Applicative, Monad)

runP :: Parser a -> String -> Except ParseError a
runP p s = undefined

pChar :: Parser Char
pChar = P $ ES $ \(pos, s) ->
  case s of
    []     -> Error (ErrorAtPos pos)
    (c:cs) -> Success (c :# (pos + 1, cs))

parseError :: Parser a
instance Alternative Parser where
  empty = parseError
  (<|>) = undefined
parseError = undefined

instance MonadPlus Parser   -- No methods.

pEof :: Parser ()
pEof = undefined

parseExpr :: String -> Except ParseError Expr
parseExpr = undefined
