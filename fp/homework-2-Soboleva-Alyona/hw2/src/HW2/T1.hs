module HW2.T1 (
    Option(..),
    mapOption,
    Pair(..),
    mapPair,
    Quad(..),
    mapQuad,
    Annotated(..),
    mapAnnotated,
    Except(..),
    mapExcept,
    Prioritised(..),
    mapPrioritised,
    Stream(..),
    mapStream,
    List(..),
    mapList,
    Fun(..), 
    mapFun,
    Tree(..),
    mapTree
) where

data Option a = None | Some a
mapOption      :: (a -> b) -> (Option a -> Option b)
mapOption _ None = None
mapOption f (Some a) = Some $ f a 

data Pair a = P a a
mapPair        :: (a -> b) -> (Pair a -> Pair b)
mapPair f (P a1 a2) = P (f a1) (f a2)


data Quad a = Q a a a a
mapQuad        :: (a -> b) -> (Quad a -> Quad b)
mapQuad f (Q a1 a2 a3 a4) = Q (f a1) (f a2) (f a3) (f a4)

data Annotated e a = a :# e
infix 0 :#
mapAnnotated   :: (a -> b) -> (Annotated e a -> Annotated e b)
mapAnnotated f (a :# e) = (f a) :# e

data Except e a = Error e | Success a
mapExcept      :: (a -> b) -> (Except e a -> Except e b)
mapExcept _ (Error e) = Error e 
mapExcept f (Success a) = Success (f a)

data Prioritised a = Low a | Medium a | High a
mapPrioritised :: (a -> b) -> (Prioritised a -> Prioritised b)
mapPrioritised f (Low a) = Low (f a)
mapPrioritised f (Medium a) = Medium (f a)
mapPrioritised f (High a) = High (f a)

-- data Stream a = StreamConstractor a (Stream a)
data Stream a = a :> Stream a 
infixr 5 :>
mapStream      :: (a -> b) -> (Stream a -> Stream b)
mapStream f (a :> as) = (f a) :> (mapStream f as)

data List a = Nil | a :. List a
infixr 5 :.
mapList        :: (a -> b) -> (List a -> List b)
mapList _ Nil = Nil
mapList f (a :. la) = (f a) :. (mapList f la)

data Fun i a = F (i -> a)
mapFun         :: (a -> b) -> (Fun i a -> Fun i b)
mapFun f (F ia) = F (f . ia)

data Tree a = Leaf | Branch (Tree a) a (Tree a)
mapTree        :: (a -> b) -> (Tree a -> Tree b)
mapTree _ Leaf = Leaf
mapTree f (Branch Leaf a Leaf) = Branch Leaf (f a) Leaf
mapTree f (Branch t1 a t2) = Branch (mapTree f t1) (f a) (mapTree f t2) 