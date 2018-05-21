module Evaluation where

import Control.Monad

data Expr = Lit Double
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Sqrt Expr
    deriving (Show, Eq)

eval :: Expr -> Either String Double
eval (Lit n) = return n
eval (Add e1 e2) = do v1 <- eval e1
                      v2 <- eval e2
                      return (v1 + v2)
eval (Sub e1 e2) = do v1 <- eval e1
                      v2 <- eval e2
                      return (v1 - v2)
eval (Mul e1 e2) = do v1 <- eval e1
                      v2 <- eval e2
                      return (v1 * v2)
eval (Div e1 e2) = do v1 <- eval e1
                      v2 <- eval e2
                      if v2 == 0 then Left "ERROR: divide zero" else return (v1 / v2)
eval (Sqrt n) = do v <- eval n
                   if v < 0 then Left "ERROR: Sqrt negative" else return (sqrt v)