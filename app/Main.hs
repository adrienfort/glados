module Main (main) where

import System.Environment (getArgs)
import CPT.Cpt

main :: IO ()
main = do
    args <- getArgs
    content <- readFile (head args)
    print $ parse content