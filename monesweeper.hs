module Minesweeper (annotate) where

import Data.Char (intToDigit)

annotate :: [String] -> [String]
annotate board = [[fn i j | j <- [0 .. cols - 1]] | i <- [0 .. rows - 1]]
  where
    rows = length board
    cols = length (board !! 0)
    fn = mapping board

countMine :: [String] -> Int -> Int -> Int
countMine board x y
  | x < 0 || y < 0 || x >= length board || y >= length (board !! 0) = 0
  | board !! x !! y == '*' = 1
  | otherwise = 0

countMines :: [String] -> Int -> Int -> Int
countMines board x y = sum [countMine board (x + i) (y + j) | i <- [-1 .. 1], j <- [-1 .. 1]]

numToChar :: Int -> Char
numToChar n
  | n == 0 = ' '
  | otherwise = intToDigit n

mapping :: [String] -> Int -> Int -> Char
mapping board x y
  | board !! x !! y == '*' = '*'
  | otherwise = numToChar $ countMines board x y