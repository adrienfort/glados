module Eval
    (
        exec,
        ifcondition,
        add,
        minus,
        mult,
        division,
        modulo,
        inferiorto
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


-- load args into env -> pop stack with nbr args -> exec again -> add exec result ontop of stack
transitFunc :: (Int, String) -> Stack -> Env -> Either Stack String
transitFunc (l, s) stack env = Right "err"

-- checkBuiltin or Func :
-- Builtin => get builtin -> -> call builtin
-- Func => get definition -> -> send to transitFunc
call :: (Int, String) -> Stack -> Env -> Either Stack String
call (l, s) stack env = Right "err"

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


division :: Function
division (AstInteger a : AstInteger b : rest)
    | a == 0 = Right "Divide by zero in function division"
    | otherwise = Left (AstInteger (b `div` a) : rest)
division _ = Right "Error in the size of stack in division"


modulo :: Function
modulo (AstInteger a : AstInteger b : rest)
    | a == 0 = Right "Divide by zero in function modulo"
    | otherwise = Left (AstInteger (b `mod` a) : rest)
modulo _ = Right "Error in the size of stack in modulo"


inferiorto :: Function
inferiorto (AstInteger a : AstInteger b : rest)
    | b < a = Left (AstBoolean "#t" : rest)
    | otherwise = Left (AstBoolean "#f" : rest)
inferiorto _ = Right "Error in the size of stack in inferiorto"




-------------------------------- BUILTINS --------------------------------


-- type Env = [(String, Either ([String], [Instruction]) Ast)]

-- getEnvValue :: String -> Env -> Stack -> Either 


exec :: [Instruction] -> Env -> Stack -> Either Ast String
exec (Instruction {line = _, command = "push", value = Just v}:b) env stack = case push v stack of
    (Left newstack) -> exec b env newstack
    (Right err) -> Right err
exec (Instruction {line = l, command = "call", value = Just (AstSymbol s)}:b) env stack = case call (l, s) stack env of
    (Right err) -> Right err
    (Left newstack) -> exec b env newstack
exec (Instruction {line = _, command = "return", value = Nothing}:_) _ (val:stack) = Left val

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
