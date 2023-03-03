module Prompt
    (
        prompt
    )
    where

import System.IO
import CPT.Cpt
import CptToAst
import Compile
import Eval
import Lib


countParenthesis :: String -> (Int, Int) -> (Int, Int)
countParenthesis [] e = e
countParenthesis ('(':b) (s, e) = countParenthesis b (s + 1, e)
countParenthesis (')':b) (s, e) = countParenthesis b (s, e + 1)
countParenthesis (_:b) e = countParenthesis b e

addTuple :: (Int, Int) -> (Int, Int) -> (Int, Int)
addTuple (a1, b1) (a2, b2) = (a1 + a2, b1 + b2)

readInput :: String -> (Int, Int) -> Env -> IO (String, Env)
readInput a (s, e) env = case s == e of
    False -> getLine>>= \line ->
        readInput (a ++ " " ++ line)  (addTuple (countParenthesis (line) (0, 0)) (s, e)) env
    True -> case instructions of
        Right err -> return (err, nenv)
        Left [] -> return ("", nenv)
        Left li -> case exec li nenv [] of
            Right err -> return (err, nenv)
            Left value -> return (show value, nenv)
        where
            (instructions, line, nenv) = compile (startCptToAst (parse a)) 0 env

prompt :: Env -> IO ()
prompt env = isEOF >>= \eof -> case eof of
    True -> return ()
    False -> getLine >>= \line ->
        readInput line (countParenthesis line (0, 0)) env
        >>= \result -> case result of
            (out, nenv) -> putStrLn out >> prompt nenv
