module Eval.Eval
    (
        eval
    ) where

data Ast = Integer Int | Symbol String | Boolean Bool | Call String [Ast]

eval :: Int
eval = 2