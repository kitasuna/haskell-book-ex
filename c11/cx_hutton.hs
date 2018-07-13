module Hutton where

data Expr
  = Lit Integer
  | Add Expr Expr

eval :: Expr -> Integer
eval (Lit x) = x
eval (Add x1 x2) = eval x1 + eval x2

printExpr :: Expr -> String
printExpr (Lit x) = show x
printExpr (Add x1 x2) = (printExpr x1) ++ " + " ++ (printExpr x2)
