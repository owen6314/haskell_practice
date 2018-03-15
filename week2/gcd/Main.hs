solution :: Integer -> Integer -> Integer
solution x y
  | x == y = x
  | x < y = solution x (y - x)
  | x > y = solution (x - y) y
