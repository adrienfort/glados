module Main (main) where

import System.Environment (getArgs)
import CPT.Cpt
import CptToAst
import Eval

main :: IO ()
main = do
    args <- getArgs
    content <- readFile (head args)
    printEvaluation (evaluate (startCptToAst (parse content)) [])
