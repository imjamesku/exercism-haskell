module Triangle (rows) where

rows :: Int -> [[Integer]]
rows 0 = []
rows n = take n $ iterate nextRow [1]
  where
    nextRow :: [Integer] -> [Integer]
    nextRow prev = zipWith (+) (0 : prev) (prev ++ [0])
