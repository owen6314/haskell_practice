insert :: Integer -> [Integer] -> [Integer]
insert x [] = [x]
insert x (y:ys)
  | x < y = x:y:ys
  | otherwise = y:(insert x ys)


solution :: [Integer] -> [Integer]
solution [] = []
solution (x:xs) = insert x (solution xs)




