module Prime (nth) where

nth :: Int -> Maybe Integer
nth 0 = Nothing
nth n = Just $ head $ drop (n - 1) $ filter isPrime [2 ..]

isPrime :: Integer -> Bool
isPrime n
  | n < 2 = False
  | otherwise = all (\x -> n `mod` x /= 0) [2 .. floor (sqrt (fromIntegral n))]