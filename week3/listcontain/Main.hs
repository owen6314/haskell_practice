solution :: [Integer] -> [Integer] -> Bool

solution xs ys
  | length xs > length ys = null ys || (head ys) `elem` xs && solution xs (tail ys)
  | otherwise = null xs || (head xs) `elem` ys && solution ys (tail xs)