module HW1.T4(
    tfoldr,
    treeToList
) where

import HW1.T3

tfoldr :: (a -> b -> b) -> b -> Tree a -> b
tfoldr _ res Leaf = res
tfoldr f res (Branch _ left root right) = tfoldr f (f root (tfoldr f res right)) left

treeToList :: Tree a -> [a]    -- output list is sorted
treeToList = tfoldr (:) []