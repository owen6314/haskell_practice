module Maxsubsum where

solution :: [Integer] -> Integer

-- Becomes slower and slower when length ls > 1000
-- n^2 solution
-- solution ls = maximum [maximum(scanl (+) 0 (drop n ls))| n <- [0..length ls]]

-- Becomes even slowerer with length ls increasing
-- dp solution without memorizing
{-solution (x:xs) = max (x + maxSumA xs) (maxSumB xs)

maxSumA :: [Integer] -> Integer
maxSumA (x:xs)
  | null xs = max x 0
  | otherwise = max 0 (x + maxSumA xs)

maxSumB :: [Integer] -> Integer
maxSumB (x:xs)
  | null xs = x
  | otherwise = max (maxSumB xs)  (x + maxSumA xs)-}

-- dp solution with memorizing
solution ls = maximum $ scanl f (minimum ls) ls

f :: Integer -> Integer -> Integer
f a b = max (a + b) b