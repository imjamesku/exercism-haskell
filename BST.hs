module BST
  ( BST,
    bstLeft,
    bstRight,
    bstValue,
    empty,
    fromList,
    insert,
    singleton,
    toList,
  )
where

import Data.List (foldl')

data BST a = BST {left :: BST a, right :: BST a, value :: a} | EmptyTree deriving (Eq, Show)

bstLeft :: BST a -> Maybe (BST a)
bstLeft EmptyTree = Nothing
bstLeft (BST EmptyTree r v) = Just EmptyTree
bstLeft (BST l@(BST _ _ _) r v) = Just l

bstRight :: BST a -> Maybe (BST a)
bstRight EmptyTree = Nothing
bstRight (BST _ EmptyTree _) = Just EmptyTree
bstRight (BST _ r@(BST _ _ _) v) = Just r

bstValue :: BST a -> Maybe a
bstValue EmptyTree = Nothing
bstValue (BST l r v) = Just v

empty :: BST a
empty = EmptyTree

fromList :: Ord a => [a] -> BST a
fromList xs = foldl' (flip insert) EmptyTree xs

insert :: Ord a => a -> BST a -> BST a
insert x EmptyTree = singleton x
insert x t@(BST l r v)
  | x <= v = t {left = insert x l}
  | otherwise = t {right = insert x r}

singleton :: a -> BST a
singleton x = BST EmptyTree EmptyTree x

toList :: BST a -> [a]
toList = toList' []
  where
    toList' acc EmptyTree = acc
    toList' acc (BST l r v) = toList' (v : toList' acc r) l
