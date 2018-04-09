module Maxsubsum where

solution :: [Integer] -> Integer

-- recursive solution
{-solution (x:xs) = max (x + maxSumA xs) (maxSumB xs)

maxSumA :: [Integer] -> Integer
maxSumA (x:xs)
  | null xs = max x 0
  | otherwise = max 0 (x + maxSumA xs)

maxSumB :: [Integer] -> Integer
maxSumB (x:xs)
  | null xs = x
  | otherwise = max (maxSumB xs)  (x + maxSumA xs)-}

-- n^2 solution
-- solution ls = maximum [maximum(scanl (+) 0 (drop n ls))| n <- [0..length ls]]

-- dp solution
solution ls = maximum $ scanl f (minimum ls) ls

f :: Integer -> Integer -> Integer
f a b = max (a + b) b
