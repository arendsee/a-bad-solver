module BadSolver
    ( someFunc
    ) where

import BadSolver.Data

someFunc :: IO ()
someFunc = putStrLn (show (And (Var 'a') (Var 'b')))
