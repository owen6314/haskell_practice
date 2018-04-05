module Redefhead where

solution :: [a] -> a

solution ls = foldr1 solution' ls  

solution' :: a -> b -> a

solution' a b = a

