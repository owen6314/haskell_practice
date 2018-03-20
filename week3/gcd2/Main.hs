solution :: [Integer] -> Integer

solution [t] = t
solution (x:xs) = gcd2 x (solution xs)

gcd2 x y
  | x == y = x
  | x < y = gcd2 (y - x) x
  | y < x = gcd2 (x - y) y