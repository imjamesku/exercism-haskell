module Change (findFewestCoins) where

import Data.Array
import Data.Function (on)
import Data.List (minimumBy)
import Data.Maybe (catMaybes)

findFewestCoins :: Integer -> [Integer] -> Maybe [Integer]
findFewestCoins 0 _ = Just []
findFewestCoins n _ | n < 0 = Nothing
findFewestCoins target coins = memo ! target
  where
    memo = listArray (0, target) [findFewestCoins' i coins | i <- [0 .. target]]
    findFewestCoins' :: Integer -> [Integer] -> Maybe [Integer]
    findFewestCoins' target coins = if null combinations then Nothing else Just $ takeShortest combinations
      where
        takeShortest = minimumBy (compare `on` length)
        combinations = catMaybes [if c == target then Just [c] else if c > target then Nothing else (c :) <$> memo ! (target - c) | c <- coins]
