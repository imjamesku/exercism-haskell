module SumOfMultiples (sumOfMultiples) where

import Data.List (foldl')
import Data.Set qualified as Set

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = (sum . Set.toList . foldl' fn Set.empty) factors
  where
    fn set factor = Set.union set (Set.fromList (takeWhile (< limit) [factor * n | n <- [1 ..]]))
