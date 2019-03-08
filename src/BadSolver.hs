module BadSolver
(
    satisfiable
  , simplify
  , Expr(..)
) where

import Control.Applicative ((<|>))

data Expr
  = Var Char
  | And Expr Expr
  | Or Expr Expr
  | Not Expr
  | Const Bool
  deriving(Show, Eq)

-- eventually I'll have more than one algorithm for solving SAT problems
-- for now I just have a brute force solution
satisfiable = bruteSat

bruteSat :: Expr -> Bool
bruteSat e = case firstFreeVariable e of
  Nothing -> case simplify e of
    (Const x) -> x
    _ -> error "shit happened"
  (Just x) -> (bruteSat $ guess x True e) ||
              (bruteSat $ guess x False e)

firstFreeVariable :: Expr -> Maybe Char
firstFreeVariable (Var x) = Just x
firstFreeVariable (And x y) = (firstFreeVariable x) <|> (firstFreeVariable y)
firstFreeVariable (Or x y) = (firstFreeVariable x) <|> (firstFreeVariable y)
firstFreeVariable (Not x) = firstFreeVariable x
firstFreeVariable (Const _) = Nothing

guess :: Char -> Bool -> Expr -> Expr
guess c v (Var c') = case c == c' of
  True -> Const v
  False -> Var c'
guess c v (And x y) = And (guess c v x) (guess c v y)
guess c v (Or x y) = Or (guess c v x) (guess c v y)
guess c v (Not x) = Not (guess c v x)
guess _ _ (Const x) = Const x

-- simplify an expression
simplify :: Expr -> Expr
-- simplify constants over AND
simplify (And (Const x) (Const y)) = Const (x && y)
simplify (And x (Const True)) = simplify x
simplify (And (Const True) x) = simplify x
simplify (And (Const False) _) = Const False
simplify (And _ (Const False)) = Const False
-- simplify constants over OR
simplify (Or (Const x) (Const y)) = Const (x || y)
simplify (Or _ (Const True)) = Const True
simplify (Or (Const True) _) = Const True
simplify (Or (Const False) x) = simplify x
simplify (Or x (Const False)) = simplify x
-- negate constants
simplify (Not (Const x)) = Const (not x)
-- remove double negatives
simplify (Not (Not x)) = x
-- propagate
simplify (Not x  ) = simplify' $ Not (simplify x)
simplify (And x y) = simplify' $ And (simplify x) (simplify y)
simplify (Or  x y) = simplify' $ Or  (simplify x) (simplify y)
simplify x = x

-- simplify without recursion
-- technically, I could use the recursive simplify, but that would inefficient
simplify' :: Expr -> Expr
simplify' (And (Const x) (Const y)) = Const (x && y)
simplify' (And x (Const True)) = x
simplify' (And (Const True) x) = x
simplify' (And (Const False) _) = Const False
simplify' (And _ (Const False)) = Const False
simplify' (Or (Const x) (Const y)) = Const (x || y)
simplify' (Or _ (Const True)) = Const True
simplify' (Or (Const True) _) = Const True
simplify' (Or (Const False) x) = x
simplify' (Or x (Const False)) = x
simplify' (Not (Const x)) = Const (not x)
simplify' (Not (Not x)) = x
simplify' x = x
