solution :: Integer -> Integer -> Integer
solution a n
  | n < 0 = -1
  | n == 1 = a
  | mod a 2 == 1 = solution (3 * a + 1) (n - 1)
  | otherwise = solution (div a 2) (n - 1)

