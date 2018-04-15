module HuffDecode where

solution :: Ord a => [(a, [Char])] -> [Char] -> [a]
solution dic src = decode dic src [] 1

decode :: Ord a => [(a, [Char])] -> String -> [a] -> Int -> [a]
decode dic src result n
  | src == [] = result
  | snd (findInDic dic (take n src)) == False = decode dic src result (n + 1)
  | otherwise = decode dic (drop n src) (result ++ [fst (findInDic dic (take n src))]) 1

findInDic :: (Ord a) => [(a, [Char])] -> String -> (a,Bool)
findInDic dic x
  | dic == [] = ((fst $ head dic), False)
  | (snd $ head dic) == x = ((fst $ head dic),True)
  | otherwise = findInDic (drop 1 dic) x