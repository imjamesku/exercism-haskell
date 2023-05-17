module DNA (nucleotideCounts, Nucleotide (..)) where

import Data.List (foldl')
import Data.Map (Map, alter, fromList, insertWith)

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts = foldl' fn (Right $ fromList [(A, 0), (C, 0), (G, 0), (T, 0)])
  where
    fn (Left _) _ = Left "error"
    fn (Right m) x
      | x == 'A' = Right $ insertWith (+) A 1 m
      | x == 'T' = Right $ insertWith (+) T 1 m
      | x == 'G' = Right $ insertWith (+) G 1 m
      | x == 'C' = Right $ insertWith (+) C 1 m
      | otherwise = Left "error"