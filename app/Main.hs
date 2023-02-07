module Main (main) where

import CptToAst
import Eval.Eval

main :: IO ()
-- main = printCpt (CptLists [CptSymbols "define", CptSymbols "x", CptInteger 5])
-- main = compareCpts (CptLists [CptSymbols "define", CptSymbols "x", CptInteger 5]) (CptLists [CptSymbols "define", CptSymbols "x", CptInteger 5])
main = printAst (AstDefine (Left "x") (AstInteger 1))
-- main = printEvaluation (evaluate [Ast] [])