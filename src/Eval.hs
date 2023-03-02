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

-- add a new key value
-- envAddKey :: String -> Ast -> Env -> Env
-- envAddKey str ast env = (str, ast) : env

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


push :: Ast -> Stack -> Either Stack String
push ast stack = case ast of
    (AstInteger _) -> Left (ast:stack)
    (AstBoolean _) -> Left (ast:stack)
    (_) -> Right "Error dans push"


-- load args into env -> pop stack with nbr args -> exec again -> add exec result ontop of stack
transitFunc :: (Int, String) -> Stack -> Env -> Either Stack String
transitFunc (l, s) stack env = Right "err"

-- checkBuiltin or Func :
-- Builtin => get builtin -> -> call builtin
-- Func => get definition -> -> send to transitFunc
call :: (Int, String) -> Stack -> Env -> Either Stack String
call (l, s) stack env = Right "err"

exec :: [Instruction] -> Env -> Stack -> Either Ast String
exec (Instruction {line = l, command = "push", value = Just v}:b) env stack = case push v stack of
    (Left newstack) -> exec b env newstack
    (Right err) -> Right err
exec (Instruction {line = l, command = "call", value = Just (AstSymbol s)}:b) env stack = case call (l, s) stack env of
    (Right err) -> Right err
    (Left newstack) -> exec b env newstack
-- exec (Instruction {line = l, command = "get", value = Just v}:b) env stack = case push v stack of


-- type Env = [(String, Either ([String], [Instruction]) Ast)]
-- insertToTupleArray :: [(String, a)] -> String -> a -> [(String, a)]

setArgToEnv :: [String] -> Stack -> Env -> Env
setArgToEnv [] [] env = env
setArgToEnv [] _ env = env
setArgToEnv _ [] env = env
setArgToEnv (name:ns) (val:vs) env = setArgToEnv ns vs (addToTupleArray env name (Right val))

extractNFromList :: [a] -> Int -> Either ([a], [a]) String
extractNFromList l n = case length l >= n of
    True -> Left (take n l, drop n l)
    False -> Right "Not enough elements from list"