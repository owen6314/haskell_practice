build :: Eq a => [a] -> [[a]] -> [[a]]
build x all@(y:ys)
  | x == [] = reverse all
  | (head x) `elem` y || null y = build (drop 1 x) ((head x:y):ys) 
  | otherwise = build (drop 1 x) ([head x]:all)


solution :: Eq a => [a] -> [[a]]
solution x = build x [[]]


