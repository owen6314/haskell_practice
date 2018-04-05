module Bachelor where
import Data.List
solution :: [Integer] -> Integer

solution ls = head $ foldl solution' [] ls

solution' l a = if a `elem` l then delete a l else a:l