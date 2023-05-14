module Queens (boardString, canAttack) where

import Data.List (intercalate)

boardString :: Maybe (Int, Int) -> Maybe (Int, Int) -> String
boardString white black = unlines ([(unwords [fn i j | j <- [0 .. 7]]) | i <- [0 .. 7]])
  where
    fn i j
      | Just (i, j) == white = "W"
      | Just (i, j) == black = "B"
      | otherwise = "_"

canAttack :: (Int, Int) -> (Int, Int) -> Bool
canAttack (x1, y1) (x2, y2)
  | abs (x1 - x2) == abs (y1 - y2) || x1 == x2 || y1 == y2 = True
  | otherwise = False
