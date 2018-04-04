module Comgen where
import Data.List

solution :: Integer -> Integer -> [[Integer]]
solution n k = sort (solution' n k)

solution' :: Integer -> Integer -> [[Integer]]
solution' n k 
  | n == k = [[1..n]]
  | k == 1 = [[x] | x <- [1..n]]
  | otherwise = solution' (n - 1) k ++ [xs ++ [n] | xs <- solution' (n - 1) (k - 1)]

