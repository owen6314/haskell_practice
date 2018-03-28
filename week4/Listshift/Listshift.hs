module Listshift where

solution :: [a] -> Integer -> [a]

solution ls k 
  | k > toInteger len = solution ls $ mod k (toInteger len)
  | k == 0 = ls
  | otherwise = solution ((last ls) : (take (len - 1) ls)) $ k - 1
  where len = length ls
