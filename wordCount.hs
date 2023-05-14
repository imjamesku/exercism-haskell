module WordCount (wordCount) where

import Data.Char
import Data.List
import Data.Map qualified as Map

wordCount :: String -> [(String, Int)]
wordCount xs = Map.toList . countWords . removeInvalidHypos . words . replace . sToLower $ xs
  where
    sToLower = map toLower
    replace = map (\c -> if not (isAlpha (c) || isDigit (c) || c == '\'') then ' ' else c)
    removeInvalidHypos = map (\w -> if head w == '\'' || last w == '\'' then filter (/= '\'') w else w)
    countWords = foldl' reducer Map.empty
      where
        reducer countMap word = Map.insertWith (+) word 1 countMap