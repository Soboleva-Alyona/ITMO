module HW1.T6(
    mcat, 
    epart
) where

import Data.Foldable (fold)

mcat :: Monoid a => [Maybe a] -> a
mcat list = extractMaybe $ fold list
  where
    extractMaybe :: Monoid a => Maybe a -> a
    extractMaybe (Just a) = a
    extractMaybe Nothing = mempty

epart :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
epart list = foldMap unpackEither list
  where
    unpackEither :: (Monoid a, Monoid b) => Either a b -> (a, b)
    unpackEither (Left a) = (a, mempty)
    unpackEither (Right b) = (mempty, b)