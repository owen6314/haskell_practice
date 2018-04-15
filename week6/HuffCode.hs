module HuffCode where
import HuffTree
import Data.List

solution :: String -> String
solution src = getHuffCode (HuffTree.buildDict $ makeFrequentTable src) src []

-- 为什么第三个[a]换成String会出错？
-- 因为dic x中x一定是Char,不能是泛型
getHuffCode :: (Ord a) => [(a, [Char])] -> [a] -> String -> String
getHuffCode dic src curr
  | src == [] = curr
  | otherwise = let x = head src in getHuffCode dic (tail src) (curr ++ (findInDic dic x))

findInDic :: (Ord a) => [(a, [Char])] -> a -> String
findInDic dic x
  | dic == [] = []
  | (fst $ head dic) == x = snd $ head dic
  | otherwise = findInDic (drop 1 dic) x


makeFrequentTable :: String -> [(Char, Float)]
makeFrequentTable src = let len = length src in [(head x, (fromIntegral (length x)) / (fromIntegral len))|x <- group $ sort src]