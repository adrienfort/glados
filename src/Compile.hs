module Compile
    (
        compile
    ) where

import Lib

addInstruction :: (Either [Instruction] String) -> Instruction -> ((Either [Instruction] String), Int)
addInstruction a ni = case a of
    (Right err) -> (Right err, line ni)
    (Left li) -> (Left (li ++ [ni]), line ni + 1)

astListToInstructions :: [Ast] -> Int -> ((Either [Instruction] String), Int)
astListToInstructions [] i = (Left [], i)
astListToInstructions (a:b) i = case astToInstructions a i of
    (Right err, _) -> (Right err, i)
    (Left il, index) -> case astListToInstructions b index of
        (Right err, _) -> (Right err, index)
        (Left res, ind) -> (Left (il ++ res), ind)

-- Return :
--  List of instruction OR error
--  Index of the last line + 1
callToInstructions :: Ast -> Int -> ((Either [Instruction] String), Int)
callToInstructions (AstCall []) i = (Right "? invalid call", i)
callToInstructions (AstCall (_:b)) i = astListToInstructions b i
callToInstructions _ i = (Right "? invalid call", i)

astToInstructions :: Ast -> Int -> ((Either [Instruction] String), Int)
astToInstructions (AstDefine a v) i = case astToInstructions v i of
    (Right err, ni) -> (Right err, ni)
    (li, ni) -> addInstruction li Instruction {line = ni, command = "call", value = Just (AstDefine a (AstSymbol ""))} -- a changer si probleme
astToInstructions (AstCall (AstSymbol "if":b)) i = ifToInstructions 0 (AstCall (AstSymbol "if":b)) i
astToInstructions (AstCall (a:b)) i = case callToInstructions (AstCall (a:b)) i of
    (Right err, ni) -> (Right err, ni)
    (li, ni) -> addInstruction li Instruction {line = ni, command = "call", value = Just a}
astToInstructions (AstSymbol a) i = (Left [Instruction {line = i, command = "get", value = Just (AstSymbol a)}], i + 1)
astToInstructions a i = (Left [Instruction {line = i, command = "push", value = Just a}], i + 1)

ifToInstructions :: Int -> Ast -> Int -> ((Either [Instruction] String), Int)
ifToInstructions r (AstCall (_:cond:yes:no:[])) i = case r of
    -- return
    1 -> (ifList [
        lcond, (Left [Instruction {line = ic, command = "jumpIfFalse", value = Just (AstInteger (iy + r))}]),
        lyes, Left [Instruction {line = iy, command = "return", value = Nothing}],
        lno, Left [Instruction {line = ino, command = "return", value = Nothing}]
        ], ino)
    -- no return
    0 -> (ifList [
        lcond, (Left [Instruction {line = ic, command = "jumpIfFalse", value = Just (AstInteger (iy + r))}]),
        lyes,
        lno
        ], ino)
    _ -> (Right "if invalid call", i)
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
ifToInstructions _ _ i = (Right "if invalid call", i)

compileExpression :: Ast -> Int -> ((Either [Instruction] String), Int)
compileExpression (AstCall (AstSymbol "if":b)) i = ifToInstructions 1 (AstCall (AstSymbol "if":b)) i
compileExpression a i = case astToInstructions a i of
    (Right err, _) -> (Right err, i)
    (Left il, index) -> (Left (il ++ [Instruction {line = index, command = "return", value = Nothing}]), index)

compile :: [Ast] -> Int -> ((Either [Instruction] String), Int)
compile [] i = (Left [], i)
compile (a:b) i = case compileExpression a i of
    (Right err, ni) -> (Right err, ni)
    (Left x, index) -> case compile b (index + 1) of
        (Left res, ni) -> (Left (x ++ res), ni)
        (Right err, ni) -> (Right err, ni)
