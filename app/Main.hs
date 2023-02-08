module Main (main) where

import System.Environment (getArgs)
import CPT.Cpt
import CptToAst
import Eval
import Prompt

main :: IO ()
main = getArgs >>= \args -> case length args == 0 of
    True -> prompt []
    False -> readFile (head args) >>= \content -> printEvaluation (fst (evaluate (cptToAst (parse content)) []))            
