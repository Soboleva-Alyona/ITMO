module HW1.T3(
    Tree(..),
    tsize,
    tdepth,
    tmember,
    tinsert,
    tFromList
) where

data Tree a = Leaf | Branch Int (Tree a) a (Tree a)

-- | Size of the tree, O(1).
tsize :: Tree a -> Int
tsize Leaf = 0
tsize (Branch s _ _ _) = s 

-- | Depth of the tree.
tdepth :: Tree a -> Int
tdepth Leaf = 0
tdepth (Branch _ Leaf _ Leaf) = 1
tdepth (Branch _ left _ right) = 1 + max (tdepth left) (tdepth right)

-- | Check if the element is in the tree, O(log n)
tmember :: Ord a => a -> Tree a -> Bool
tmember a (Branch _ left root right)    
    | a == root = True 
    | a < root = tmember a left
    | a > root = tmember a right 
tmember _ Leaf = False

-- -- | Insert an element into the tree, O(log n)
tinsert :: Ord a => a -> Tree a -> Tree a
tinsert a Leaf = Branch 1 Leaf a Leaf
tinsert a (Branch s left root right)
    | tmember a (Branch s left root right) = Branch s left root right -- to keep uniquness  
    | a < root = Branch (s + 1) (tinsert a left) root right 
    | a > root = Branch (s + 1) left root (tinsert a right)

-- -- | Build a tree from a list, O(n log n)
tFromList :: Ord a => [a] -> Tree a
tFromList _ = undefined