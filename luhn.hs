module Luhn (isValid) where

import Data.Char (isDigit)
import Data.Foldable (Foldable (foldl', foldr'))

isValid :: String -> Bool
isValid n
  | length spacesStriped > 1 = (sum $ doubleEverySecondDigits $ toDigits spacesStriped) `mod` 10 == 0
  | otherwise = False
  where
    spacesStriped = stripSpaces n

stripSpaces :: String -> String
stripSpaces = filter (/= ' ')

toDigits :: String -> [Integer]
toDigits "" = []
toDigits (c : s)
  | isDigit (c) = read [c] : toDigits s
  | otherwise = toDigits s

doubleEverySecondDigits :: [Integer] -> [Integer]
doubleEverySecondDigits xs = foldr' f [] (zip xs [length xs - 1, length xs - 2 ..])
  where
    f (x, i) xs = (if i `mod` 2 == 1 then doubled else x) : xs
      where
        doubled = let d = x * 2 in if d > 9 then d - 9 else d