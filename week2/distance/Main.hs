formal_one (x1,y1) (x2,y2) = abs(x1 - x2) + abs(y1 - y2)

formal_two (x1,y1) (x2,y2) = sqrt((x1 - x2) * (x1 - x2) + (y1 - y2) * (y1 - y2))

formal_infinity (x1,y1) (x2,y2) = maximum(abs(x1 - x2), abs(y1 - y2))

solution p (x1,y1) (x2,y2)
  | p == 1 = formal_one (x1,y1) (x2,y2)
  | p == 2 = formal_two (x1,y1) (x2,y2)
  | otherwise = formal_infinity (x1,y1) (x2,y2)
