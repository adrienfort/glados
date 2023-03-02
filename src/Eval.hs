module Eval
    (
        exec,
        ifcondition,
        add,
        minus,
        mult,
        division
    ) where

import Lib

stackPush :: Stack -> Ast -> Stack
stackPush a v = v : a

stackPop :: Stack -> Stack
stackPop [] = []
stackPop (_:b) = b

-- envGetKey :: String -> Env -> Either Ast String
-- envGetKey str env = case lookup str env of
        -- Nothing -> Right (str ++ " is undefined")
        -- Just val -> Left val
-- 
-- add a key value and can replace an existing key value
-- envInsertKey :: String -> Ast -> Env -> Env
-- envInsertKey str ast env = case lookup str env of
    -- Nothing -> (str, ast) : env
    -- Just _ -> replaceKey env
        -- where
            -- replaceKey [] = []
            -- replaceKey ((s, val) : b) | s == str = ((str, ast) : b)
                                    -- | otherwise = ((s, val) : (replaceKey b))


-- instructions :: [Instruction]
-- instructions = [
    -- Instruction {line = 0, command = "push", value = Just (AstInteger 0)},
    -- Instruction {line = 1, command = "get", value = Just (AstSymbol "x")},
    -- Instruction {line = 2, command = "call", value = Just (AstSymbol "eq?")},
    -- Instruction {line = 3, command = "jumpIfFalse", value = Just (AstInteger 7)},
    -- Instruction {line = 4, command = "push", value = Just (AstInteger 1)},
    -- Instruction {line = 5, command = "return", value = Nothing},
    -- Instruction {line = 6, command = "get", value = Just (AstSymbol "foo")},
    -- Instruction {line = 7, command = "return", value = Nothing}
    -- ]


stack :: Stack
stack = [
    AstInteger 10,
    AstInteger 2
    ]

jump :: [Instruction] -> Int -> [Instruction]
jump [] _ = []
jump (x:xs) lineNum
    | line x == lineNum = x:xs
    | otherwise = jump xs lineNum

-- add a new key value
-- envAddKey :: String -> Ast -> Env -> Env
-- envAddKey str ast env = (str, ast) : env

push :: Ast -> Stack -> Either Stack String
push ast stack = case ast of
    (AstInteger _) -> Left (ast:stack)
    (AstBoolean _) -> Left (ast:stack)
    (_) -> Right "Error dans push"



-------------------------------- BUILTINS --------------------------------
-- ("if", ifcondition),
-- ("+", add),
-- ("-", minus),
-- ("*", mult),
-- ("div", division),
-- ("mod", modulo),
-- ("<", inferiorto),
-- ("eq?", equal)

ifcondition :: Function
ifcondition (AstInteger a : AstInteger b : rest) = case a == b of
    (True) -> Left (AstBoolean "#t" : rest)
    (False) -> Left (AstBoolean "#f" : rest)
ifcondition (AstBoolean a : AstBoolean b : rest) = case a == b of
    (True) -> Left (AstBoolean "#t" : rest)
    (False) -> Left (AstBoolean "#f" : rest)
ifcondition _ = Right "Error in the size of stack in ifcondition"


add :: Function
add (AstInteger a : AstInteger b : rest) = Left (AstInteger (b + a) : rest)
add _ = Right "Error in the size of stack in add"

minus :: Function
minus (AstInteger a : AstInteger b : rest) = Left (AstInteger (b - a) : rest)
minus _ = Right "Error in the size of stack in minus"


mult :: Function
mult (AstInteger a : AstInteger b : rest) = Left (AstInteger (b * a) : rest)
mult _ = Right "Error in the size of stack in mult"


-- division :: Function
-- division (AstInteger a : AstInteger b : rest) =  case AstInteger a of
--     (a == 0) -> Right "Divide by zero in function division"
--     _ -> Left (AstInteger (b`div`a) : rest)
-- division _ = Right "Error in the size of stack in division"

division :: Function
division (AstInteger a : AstInteger b : rest)
    | a == 0 = Right "Divide by zero in function division"
    | otherwise = Left (AstInteger (b `div` a) : rest)
division _ = Right "Error in the size of stack in division"

-- division :: Function
-- division (AstInteger a : AstInteger b : rest) = Left (AstInteger (b `div` a) : rest)
-- division _ = Right "Error in the size of stack in division"


-------------------------------- BUILTINS --------------------------------


-- type Env = [(String, Either ([String], [Instruction]) Ast)]

-- getEnvValue :: String -> Env -> Stack -> Either 


exec :: [Instruction] -> Env -> Stack -> Either Ast String
exec (Instruction {line = l, command = "push", value = Just v}:b) env stack = case push v stack of
    (Left newstack) -> exec b env newstack
    (Right error) -> Right error
exec (Instruction {line = l, command = "return", value = Nothing}:_) env (a:b) = Left a

-- exec (Instruction {line = l, command = "get", value = Just v}:b) env stack = case push v stack of


toto :: IO()
toto = print (add stack)