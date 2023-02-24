module Compile
    (
        compile
    ) where

import Lib

-- astListsToInstructions :: [Ast] -> Int -> ((Either [Instruction] String), Int)
-- astListsToInstructions [] i = (Left [], i)
-- astListsToInstructions (a:b) i = case astToInstructions a i of
    -- (Right err, _) -> (Right err, i)
    -- (Left il, index) -> case astListsToInstructions b (index + 1) of
        -- (Right err, _) -> (Right err, index)
        -- (Left res, ind) -> (Left (il ++ res), ind + 1)

-- Return :
--  List of instruction OR error
--  Index of the last line + 1

-- define
-- lambda
-- call
astToInstructions :: Ast -> Int -> ((Either [Instruction] String), Int)
astToInstructions (AstCall (AstSymbol "if":b)) i = ifToInstructions 0 (AstCall (AstSymbol "if":b)) i
astToInstructions (AstSymbol a) i = (Left [Instruction {line = i, command = "get", value = Just (AstSymbol a)}], i + 1)
astToInstructions a i = (Left [Instruction {line = i, command = "push", value = Just a}], i + 1)

ifToInstructions :: Int -> Ast -> Int -> ((Either [Instruction] String), Int)
ifToInstructions r (AstCall (_:cond:yes:no:[])) i = case r of
    -- return
    1 -> (ifList [
        lcond, (Left [Instruction {line = ic, command = "jumpIfFalse", value = Just (AstInteger (iy + 1))}]),
        lyes, Left [Instruction {line = iy, command = "return", value = Nothing}],
        lno, Left [Instruction {line = ino, command = "return", value = Nothing}]
        ], ino + 1)
    -- no return
    0 -> (ifList [
        lcond, (Left [Instruction {line = ic, command = "jumpIfFalse", value = Just (AstInteger (iy + 1))}]),
        lyes,
        lno
        ], ino + 1)
    where
        (lcond, ic) = astToInstructions cond i -- cond 
        (lyes, iy) = astToInstructions yes (ic + 1) -- cond + jump + true
        (lno, ino) = astToInstructions no (iy + r) -- cond + jump + true + (return ?) + false
        ifList :: [(Either [Instruction] String)] -> (Either [Instruction] String)
        ifList [] = Left []
        ifList (a:b) = case a of
            (Left il) -> case ifList b of
                (Left ret) -> Left (il ++ ret)
                Right err -> Right err
            Right err -> Right err

compileExpression :: Ast -> Int -> ((Either [Instruction] String), Int)
compileExpression (AstCall (AstSymbol "if":b)) i = ifToInstructions 1 (AstCall (AstSymbol "if":b)) i
compileExpression a i = case astToInstructions a i of
    (Right err, _) -> (Right err, i)
    (Left il, index) -> (Left (il ++ [Instruction {line = index + 1, command = "return", value = Nothing}]), index + 1)

compile :: [Ast] -> Int -> (Either [Instruction] String)
compile [] _ = (Left [])
compile (a:b) i = case compileExpression a i of
    (Right b, _) -> (Right b)
    (Left x, index) -> case compile b (index + 1) of
        (Left res) -> (Left (x ++ res))
        Right err -> Right err
