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

setArgToEnv :: [String] -> Stack -> Env -> Env
setArgToEnv [] [] env = env
setArgToEnv [] _ env = env
setArgToEnv _ [] env = env
setArgToEnv (name:ns) (val:vs) env = setArgToEnv ns vs (addToTupleArray env name (Right val))

-- In case of success return (extracted values, new stack)
extractNFromList :: [a] -> Int -> Either ([a], [a]) String
extractNFromList l n = case length l >= n of
    True -> Left (take n l, drop n l)
    False -> Right "Not enough elements from list"

stackPush :: Stack -> Ast -> Stack
stackPush a v = v : a

stackPop :: Stack -> Stack
stackPop [] = []
stackPop (_:b) = b

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

-------------------------------- BUILTINS --------------------------------

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
add _ = Right "add invalid function call"


minus :: Function
minus (AstInteger a : AstInteger b : rest) = Left (AstInteger (b - a) : rest)
minus _ = Right "minus invalid function call"


mult :: Function
mult (AstInteger a : AstInteger b : rest) = Left (AstInteger (b * a) : rest)
mult _ = Right "mult invalid function call"


division :: Function
division (AstInteger a : AstInteger b : rest)
    | a == 0 = Right "division divide by zero"
    | otherwise = Left (AstInteger (b `div` a) : rest)
division _ = Right "division invalid function call"


modulo :: Function
modulo (AstInteger a : AstInteger b : rest)
    | a == 0 = Right "Divide by zero in function modulo"
    | otherwise = Left (AstInteger (b `mod` a) : rest)
modulo _ = Right "modulo invalid function call"


inferiorto :: Function
inferiorto (AstInteger a : AstInteger b : rest)
    | b < a = Left (AstBoolean "#t" : rest)
    | otherwise = Left (AstBoolean "#f" : rest)
inferiorto _ = Right "< invalid function call"

equal :: Function
equal (a:b:rest) = case a == b of
    True -> Left (AstBoolean "#t" : rest)
    False -> Left (AstBoolean "#f" : rest)
equal _ = Right "eq? invalid function call"

getBuiltins :: [(String, Function)]
getBuiltins = [
        ("+", add),
        ("-", minus),
        ("*", mult),
        ("div", division),
        ("mod", modulo),
        ("<", inferiorto),
        ("eq?", equal)
    ]

isBuiltin :: Stack -> (Int, String) -> (Bool, Either Stack String)
isBuiltin stack (l, s) = case searchTupleArray getBuiltins s of
    Nothing -> (False, Left stack)
    Just built -> (True, built stack)

isFunc :: (Int, String) -> Stack -> Env -> Either Stack String
isFunc (l, s) stack env = case searchTupleArray env s of
    Nothing -> Right (s ++ " undefined function")
    Just (Left (args, instructions)) -> case extractNFromList stack (length args) of
        Right err -> Right err
        Left (tempstack, newstack) -> case exec instructions (setArgToEnv args (reverse tempstack) env) [] of
            Right err -> Right err
            Left val -> Left (val : newstack)
    _ -> Right (s ++ " not a function")

call :: (Int, String) -> Stack -> Env -> Either Stack String
call (l, s) stack env = case isBuiltin stack (l, s) of
    (True, val) -> val
    (False, _) -> isFunc (l, s) stack env

exec :: [Instruction] -> Env -> Stack -> Either Ast String
exec (Instruction {line = _, command = "push", value = Just v}:b) env stack = case push v stack of
    (Left newstack) -> exec b env newstack
    (Right err) -> Right err
exec (Instruction {line = l, command = "call", value = Just (AstSymbol s)}:b) env stack = case call (l, s) stack env of
    (Right err) -> Right err
    (Left newstack) -> exec b env newstack
exec (Instruction {line = _, command = "return", value = Nothing}:_) _ (val:stack) = Left val

-- exec (Instruction {line = l, command = "get", value = Just v}:b) env stack = case push v stack of
