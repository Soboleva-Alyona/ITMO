module Main where

import Data.Set
import Control.Monad.Cont
import Lib
import System.Console.Haskeline
import System.Console.Haskeline.IO
import Text.Megaparsec.Error (errorBundlePretty)
import HW3.Parser
import HW3.Evaluator
import HW3.Pretty
import HW3.Base

main :: IO ()
main = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      minput <- getInputLine "hi> "
      case minput of
        Just ":quit" -> return ()
        Just input -> do
            parseInput input
            loop
        _ -> return ()

parseInput input = do
          case parse input of
            Left err -> outputStrLn $ errorBundlePretty err
            Right expr -> do
              evaluated <- eval expr
              case evaluated of
                Left err -> outputStrLn $ show err
                Right res -> outputStrLn $ show $ prettyValue res

