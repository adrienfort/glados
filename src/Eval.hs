module Eval
    (
        exec
    ) where

import Lib

stackPush :: Stack -> Ast -> Stack
stackPush a v = v : a

stackPop :: Stack -> Stack
stackPop [] = []
stackPop (_:b) = b

envGetKey :: String -> Env -> Either Ast String
envGetKey str env = case lookup str env of
        Nothing -> Right (str ++ " is undefined")
        Just val -> Left val

-- add a key value and can replace an existing key value
envInsertKey :: String -> Ast -> Env -> Env
envInsertKey str ast env = case lookup str env of
    Nothing -> (str, ast) : env
    Just _ -> replaceKey env
        where
            replaceKey [] = []
            replaceKey ((s, val) : b) | s == str = ((str, ast) : b)
                                    | otherwise = ((s, val) : (replaceKey b))

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
envAddKey :: String -> Ast -> Env -> Env
envAddKey str ast env = (str, ast) : env

push :: Ast -> Stack -> Either Stack String
push ast stack = case ast of
    (AstInteger _) -> Left (ast:stack)
    (AstBoolean _) -> Left (ast:stack)
    (_) -> Right "Error dans push"

exec :: [Instruction] -> Env -> Stack -> Either Ast String
exec (Instruction {line = l, command = "push", value = Just v}:b) env stack = case push v stack of
    (Left newstack) -> exec b env newstack
    (Right error) -> Right error
-- exec (Instruction {line = l, command = "get", value = Just v}:b) env stack = case push v stack of
