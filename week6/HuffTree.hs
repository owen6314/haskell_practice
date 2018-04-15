module HuffTree where
import Data.Function (on)
import Data.List (sortBy)

data HTree a = Leaf a | Branch (HTree a) (HTree a) deriving (Show)

buildTree :: a -> HTree a
buildTree = Leaf

mergeTree :: HTree a -> HTree a -> HTree a
mergeTree = Branch

buildHTree :: (Ord a, Ord w, Num w) => [(a,w)] -> HTree a
buildHTree ls = fst $ head $ buildFromList [(buildTree a, w) | (a,w) <- ls]

buildFromList :: (Ord a, Ord w, Num w) => [(HTree a, w)] -> [(HTree a, w)]
buildFromList ls
  | length ls == 1 = ls
  | otherwise = let ls' = sorted ls in buildFromList ((merge (head ls') (ls' !! 1)):(drop 2 ls'))

sorted :: (Ord a, Ord w, Num w) => [(HTree a, w)] -> [(HTree a, w)]
sorted ls = sortBy (compare `on` snd) ls

merge :: (Ord a, Ord w, Num w) => (HTree a, w) -> (HTree a, w) -> (HTree a, w)
merge a1 a2 = (mergeTree (fst a1) (fst a2), snd a1 + snd a2)

buildDict :: (Ord a, Ord w, Num w) => [(a, w)] -> [(a, [Char])]
buildDict ls = [(a, getCode a (buildHTree ls) [])| (a,w) <- ls]

getCode :: (Ord a) => a -> HTree a -> [Char] -> [Char]
getCode a (Leaf x) curr = if a == x then curr else []
getCode a (Branch x y) curr = (getCode a x (curr++"0")) ++ (getCode a y (curr++"1"))

{-
data Weighted a = WPair { _wItem :: a ,_wWeight :: Float} deriving (Show)

instance Eq (Weighted a) where
  WPair _ w1 == WPair _ w2 = w1 == w2

instance Ord (Weighted a) where
  compare (WPair _ w1) (WPair _ w2) = compare w1 w2-}

{- type WeightedTree a = Weighted (HTree a)

buildWeightedTree :: Float -> a -> WeightedTree a
buildWeightedTree w = WPair w . buildTree

mergeWeightedTree :: WeightedTree a -> WeightedTree a -> WeightedTree a
mergeWeightedTree (WPair w1 a1) (WPair w2 a2) = WPair (w1 + w2) (mergeTree a1 a2) -}