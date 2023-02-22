module Compile
    (
    ) where

import Lib

data Ast = Integer Int | Symbol String | Boolean Bool | Call [Ast] | Define (Either String [String]) Ast | Lambda [String] Ast

astToInstructions :: Ast -> (Either [Instruction] String)
astToInstructions _ = (Right "b")

-- Ast | Env | String
compileExpression :: Ast -> (Either [Instruction] String)
compileExpression (Call (Symbol "if":b)) = (Right "if")
compileExpression a = (Right "normal")
-- last => recupere la derniere node
-- call astToInstructions and add a return at the end
-- exception for if (if don't need return at the end, it has already one)

compile :: [Ast] -> (Either [Instruction] String)
compile [] = (Left [])
compile (a:b) = case compileExpression a of
    (Right b) -> (Right b)
    (Left a) -> case compile b of
        (Left res) -> (Left (a ++ res))
        (Right err) -> (Right err)
