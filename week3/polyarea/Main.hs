solution :: [(Double, Double)] -> Double

solution xs
  | length xs == 3 = 0.5 * abs((fst(xs !! 0) * snd(xs !! 1) + fst(xs !! 1) * snd(xs !! 2) + fst(xs !! 2) * snd(xs !! 0) - fst(xs !! 0) * snd(xs !! 2) - fst(xs !! 1) * snd(xs !! 0) - fst(xs !! 2) * snd(xs !! 1)))
  | otherwise = solution (take 3 xs) + solution ((head xs) : drop 2 xs)