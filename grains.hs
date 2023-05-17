module Grains (square, total) where

square :: Integer -> Maybe Integer
square n
  | n > 64 || n < 1 = Nothing
  | otherwise = Just $ 2 ^ (n - 1)

total :: Integer
total = sum $ map (\x -> 2 ^ (x - 1)) [1 .. 64]
