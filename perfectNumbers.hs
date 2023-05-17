module PerfectNumbers (classify, Classification (..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify n
  | n <= 0 = Nothing
  | otherwise =
      case compare (sum $ factors n) n of
        LT -> Just Deficient
        EQ -> Just Perfect
        GT -> Just Abundant
  where
    factors n = [x | x <- [1 .. n `div` 2], n `mod` x == 0]
