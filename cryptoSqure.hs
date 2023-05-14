module CryptoSquare (encode) where

import Data.Foldable
import Data.List (intercalate, transpose)

encode :: String -> String
encode xs = toEncodedText (transposeRectangle $ toRectangle xs rxc) rxc
  where
    len = length $ removeSpacesAndPunctuation xs
    rxc = findRXC len

removeSpacesAndPunctuation :: String -> String
removeSpacesAndPunctuation = filter (\x -> x `notElem` " ,.!?")

findRXC :: Int -> (Int, Int, Int)
findRXC len
  | isSquare len = (floor sqrtN, floor sqrtN, 0)
  | otherwise =
      let lo = floor sqrtN
          hi = ceiling sqrtN
       in (lo, hi, lo * hi - len)
  where
    sqrtN = sqrt (fromIntegral len)

toRectangle :: String -> (Int, Int, Int) -> [String]
toRectangle xs (r, x, c) = reverse $ foldl' reducer [] withIdx
  where
    withIdx = zip xs [0 .. length xs - 1]
    reducer :: [String] -> (Char, Int) -> [String]
    reducer block (ch, idx) =
      if idx `mod` c == 0
        then [ch] : block
        else let (x : xs) = block in (x ++ [ch]) : xs

transposeRectangle :: [String] -> [String]
transposeRectangle = transpose

toEncodedText :: [String] -> (Int, Int, Int) -> String
toEncodedText xs (r, x, c) = (intercalate separator $ xs) ++ separator
  where
    separator = replicate (c - x) ' '

isSquare :: Integral a => a -> Bool
isSquare n = (round sqrtN) ^ 2 == n
  where
    sqrtN = sqrt $ fromIntegral n