module Main (main) where

import System.Environment (getArgs)
import CPT.Cpt

main :: IO ()
main = do
    args <- getArgs
    print $ parse (head args)