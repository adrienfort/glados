module Prompt
    (
        prompt
    )
    where

import System.IO
import CPT.Cpt
import CptToAst
import Eval


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
        readInput (a ++ line)  (addTuple (countParenthesis (line ++ " ") (0, 0)) (s, e)) env
    True -> case evaluate (cptToAst (parse a)) env of
        (Environment nenv) -> return ("", nenv)
        res -> return (show res, env)

prompt :: Env -> IO ()
prompt env = isEOF >>= \eof -> case eof of
    True -> return ()
    False -> getLine >>= \line ->
        readInput line (countParenthesis line (0, 0)) env
        >>= \result -> case result of
            (out, nenv) -> case out of
                "" -> prompt nenv
                a -> putStrLn a >> prompt nenv
