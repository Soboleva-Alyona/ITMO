module HW1.T5(
    splitOn, 
    joinWith
) where

import Data.List.NonEmpty

splitOn :: Eq a => a -> [a] -> NonEmpty [a]
splitOn sep = foldr (splitHelp sep) ([] :| []) where 
    splitHelp :: Eq a => a -> a -> NonEmpty [a] -> NonEmpty [a]
    splitHelp sep' cur l@(h :| t) 
            | cur == sep' = [] <| l
            | otherwise = (cur:h) :| t

joinWith :: a -> NonEmpty [a] -> [a]
joinWith _ (h:|[]) = h
joinWith sep (h:| t) = h ++ [sep] ++ joinWith sep ([]:|t) 