module Main (main) where

import System.Environment (getArgs)
import CPT.Cpt
import CptToAst
import Compile
import Eval
import Prompt

main :: IO ()
main = getArgs >>= \args -> case length args == 0 of
    True -> prompt []
    False -> readFile (head args) >>= \content -> case compile (startCptToAst (parse content)) 0 [] of
        (Right err, _, _) -> print err
        (Left li, _, nenv) -> case exec li nenv [] of
            Right err -> putStrLn err
            Left value -> putStrLn (show value)
