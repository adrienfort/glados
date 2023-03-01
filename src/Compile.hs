module Compile
    (
        compile
    ) where

import Lib

appendInstruction :: (Either [Instruction] String) -> [Instruction] -> Env -> ((Either [Instruction] String), Int, Env)
appendInstruction a ni env = case a of
    (Right err) -> (Right err, line (last ni), env)
    (Left li) -> (Left (li ++ ni), line (last ni) + 1, env)

astListToInstructions :: [Ast] -> Int -> Env -> ((Either [Instruction] String), Int, Env)
astListToInstructions [] i env = (Left [], i, env)
astListToInstructions (a:b) i env = case astToInstructions a i env of
    (Right err, _, nenv) -> (Right err, i, nenv)
    (Left il, index, nenv) -> case astListToInstructions b index nenv of
        (Right err, _, nnenv) -> (Right err, index, nnenv)
        (Left res, ind, nnenv) -> (Left (il ++ res), ind, nnenv)

callToInstructions :: Ast -> Int -> Env -> ((Either [Instruction] String), Int, Env)
callToInstructions (AstCall []) i env = (Right "? invalid call", i, env)
callToInstructions (AstCall (_:b)) i env = astListToInstructions b i env
callToInstructions _ i env = (Right "? invalid call", i, env)

astToInstructions :: Ast -> Int -> Env -> ((Either [Instruction] String), Int, Env)
-- Lambda format :
-- Get instructions from expression -> Add lambda instructions to Env -> Call Lambda -> deleteEnv lambda
astToInstructions (AstLambda args v) i env = case astToInstructions v i env of
    (Right err, ni, nenv) -> (Right err, ni, nenv)
    (Left li, ni, nenv) -> appendInstruction (Left li) [Instruction {line = ni, command = "call", value = Just (AstSymbol "lambda")},
        Instruction {line = ni + 1, command = "deleteEnv", value = Just (AstSymbol "lambda")}] (nenv ++ [("lambda", Left (args, li))]) -- to tail
astToInstructions (AstCall (AstSymbol "if":b)) i env = ifToInstructions i (AstCall (AstSymbol "if":b)) i env
astToInstructions (AstCall (a:b)) i env = case callToInstructions (AstCall (a:b)) i env of
    (Right err, ni, nenv) -> (Right err, ni, nenv)
    (li, ni, nenv) -> appendInstruction li [Instruction {line = ni, command = "call", value = Just a}] nenv
astToInstructions (AstSymbol a) i env = (Left [Instruction {line = i, command = "get", value = Just (AstSymbol a)}], i + 1, env)
astToInstructions (AstDefine _ _) i env = (Right "define invalid call", i, env)
astToInstructions a i env = (Left [Instruction {line = i, command = "push", value = Just a}], i + 1, env)

ifToInstructions :: Int -> Ast -> Int -> Env -> ((Either [Instruction] String), Int, Env)
ifToInstructions r (AstCall (_:cond:yes:no:[])) i env = case r of
    1 -> (ifList [
        lcond, (Left [Instruction {line = ic, command = "jumpIfFalse", value = Just (AstInteger (iy + r))}]),
        lyes, Left [Instruction {line = iy, command = "return", value = Nothing}],
        lno, Left [Instruction {line = ino, command = "return", value = Nothing}]], ino + 1, nenv)
    0 -> (ifList [
        lcond, (Left [Instruction {line = ic, command = "jumpIfFalse", value = Just (AstInteger (iy + r))}]),
        lyes, lno], ino, nenv)
    _ -> (Right "if invalid call", i, env)
    where
        (lcond, ic, cenv) = astToInstructions cond i env -- cond 
        (lyes, iy, yenv) = astToInstructions yes (ic + 1) cenv -- cond + jump + true
        (lno, ino, nenv) = astToInstructions no (iy + r) yenv -- cond + jump + true + (return ?) + false
        ifList :: [(Either [Instruction] String)] -> (Either [Instruction] String)
        ifList [] = Left []
        ifList (a:b) = case a of
            (Left il) -> case ifList b of
                (Left ret) -> Left (il ++ ret)
                Right err -> Right err
            Right err -> Right err
ifToInstructions _ _ i env = (Right "if invalid call", i, env)

defineInstruction :: Ast -> Int -> Env -> ((Either [Instruction] String), Int, Env)
-- define lambda = env
defineInstruction (AstDefine (Left s) (AstLambda args v)) i env = case compileExpression v 0 env of
    (Right err, _, nenv) -> (Right err, i, nenv)
    (Left a, _, nenv) -> (Left [], i, insertToTupleArray nenv s (Left (args, a)))
-- define var = define
defineInstruction (AstDefine (Left s) v) i env = case compileExpression v i env of
    (Right err, ni, nenv) -> (Right err, ni, nenv)
    (li, ni, nenv) -> (li, ni, nenv) -- call guillaume exec function
-- invalid define func
defineInstruction (AstDefine (Right []) _) i env = (Right "define invalid call", i, env)
-- define func = env
defineInstruction (AstDefine (Right (s:b)) v) i env = case compileExpression v 0 env of
    (Right err, _, nenv) -> (Right err, i, nenv)
    (Left a, _, nenv) -> (Left [], i, insertToTupleArray nenv s (Left (b, a)))
defineInstruction _ i env = (Right "? invalid call", i, env)

compileExpression :: Ast -> Int -> Env -> ((Either [Instruction] String), Int, Env)
compileExpression (AstDefine a v) i env = defineInstruction (AstDefine a v) i env
compileExpression (AstCall (AstSymbol "if":b)) i env = ifToInstructions 1 (AstCall (AstSymbol "if":b)) i env
compileExpression a i env = case astToInstructions a i env of
    (Right err, _, nenv) -> (Right err, i, nenv)
    (Left il, index, nenv) -> (Left (il ++ [Instruction {line = index, command = "return", value = Nothing}]), index + 1, nenv)

compile :: [Ast] -> Int -> Env -> ((Either [Instruction] String), Int, Env)
compile [] i env = (Left [], i, env)
compile (a:b) i env = case compileExpression a i env of
    (Right err, ni, nenv) -> (Right err, ni, nenv)
    (Left x, index, nenv) -> case compile b index nenv of
        (Left res, ni, nnenv) -> (Left (x ++ res), ni, nnenv)
        (Right err, ni, nnenv) -> (Right err, ni, nnenv)
