{-# LANGUAGE InstanceSigs #-}

module HW1.T7(
    ListPlus(..),
    Inclusive(..),
    DotString(..),
    Fun(..)
) where

data ListPlus a = a :+ ListPlus a | Last a
infixr 5 :+
instance Semigroup (ListPlus a) where
  (<>) :: ListPlus a -> ListPlus a -> ListPlus a
  (<>) (Last a1) (Last a2) = a1 :+ (Last a2)
  (<>) (Last a1) list = a1 :+ list
  (<>) (a1 :+ xs) (Last a2) = a1 :+ (xs <> (Last a2))
  (<>) (a1 :+ xs1) list2 = a1 :+ (xs1 <> list2)

data Inclusive a b = This a | That b | Both a b
instance (Semigroup a, Semigroup b) => Semigroup (Inclusive a b) where
  (<>) :: Inclusive a b -> Inclusive a b -> Inclusive a b
  (<>) (This a) (This b) = This (a <> b)
  (<>) (That a) (That b) = That (a <> b)
  (<>) (This a) (That b) = Both a b
  (<>) (That a) (This b) = Both b a
  (<>) (Both a b) (This c) = Both (a <> c) b
  (<>) (This c) (Both a b) = Both (c <> a) b
  (<>) (Both a b) (That c) = Both a (b <> c)
  (<>) (That c) (Both a b) = Both a (c <> b)
  (<>) (Both a b) (Both c d) = Both (a <> c) (b <> d)

newtype DotString = DS String -- deriving (Show)
instance Semigroup (DotString) where
  (<>) :: DotString -> DotString -> DotString
  (<>) (DS "") othr  = othr
  (<>) othr (DS "") = othr
  (<>) (DS s1) (DS s2) = DS $ s1 ++ "." ++ s2
instance Monoid (DotString) where
  mempty:: DotString
  mempty = DS ""

newtype Fun a = F (a -> a)
instance Semigroup (Fun a) where
  (<>) :: Fun a -> Fun a -> Fun a
  (<>) (F a1) (F a2) = F $ a1 . a2
instance Monoid (Fun a) where
  mempty :: Fun a
  mempty = F id