module Main where

import BadSolver

main :: IO ()
main = do
  print $ satisfiable (And (Var 'x') (Not (Var 'x')))
