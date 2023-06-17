import Data.Array

fib :: Int -> Integer
fib n = fibs ! n
  where
    fibs = listArray (0, n) [fib' i | i <- [0 .. n]]

fib' :: Int -> Integer
fib' 0 = 0
fib' 1 = 1
fib' n = fib (n - 1) + fib (n - 2)