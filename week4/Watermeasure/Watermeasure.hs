module Watermeasure where

solution :: Integer -> Integer -> Integer -> Integer
solution a b c = getSolution [] [(0,0,0)] a b c

getSolution :: [(Integer,Integer, Integer)] -> [(Integer,Integer,Integer)] -> Integer -> Integer -> Integer -> Integer
getSolution old arr a b c
  | arr == [] = -1
  | first (head arr) == c || second (head arr)  == c = third (head arr)
  | otherwise = getSolution (head arr : old) (tail arr ++ (removeDuplicate (getNext (first $ head arr) (second $ head arr) a b (third $ head arr)) old)) a b c

removeDuplicate :: [(Integer, Integer, Integer)] -> [(Integer, Integer, Integer)] -> [(Integer, Integer, Integer)]
removeDuplicate xs old 
  | xs == [] = []
  | otherwise = (if isElemOf a old then removeDuplicate (tail xs) old else a : removeDuplicate (tail xs) old) where a = head xs

isElemOf :: (Integer, Integer, Integer) -> [(Integer, Integer, Integer)] -> Bool
isElemOf a old
  | old == [] = False
  | first a == first (head old) && second a == second (head old) = True
  | otherwise = isElemOf a $ tail old


getNext :: Integer -> Integer -> Integer -> Integer -> Integer -> [(Integer, Integer, Integer)]
getNext c_a c_b a b t= [(0, c_b, t + 1), (c_a, 0, t + 1), (a, c_b, t + 1), (c_a, b, t + 1), ((max 0 (c_a + c_b - b)) ,(min b (c_a + c_b)), t + 1), ((min a (c_a + c_b)) ,(max 0 (c_a + c_b - a)), t + 1)]

first :: (Integer, Integer, Integer) -> Integer   
first (x, _, _) = x   
 
second :: (Integer, Integer, Integer) -> Integer  
second (_, y, _) = y   
  
third :: (Integer, Integer, Integer) -> Integer   
third (_, _, z) = z 