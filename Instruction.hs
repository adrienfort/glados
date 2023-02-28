data Ast = AstInteger Int
    | AstSymbol String
    | AstBoolean String
    | AstCall [Ast]
    | AstDefine (Either String [String]) Ast
    | AstLambda [String] Ast deriving (Show)

data Instruction = Instruction {
    line :: Int,
    command :: String,
    value :: Maybe Ast
}


type Stack = [Ast]

instance Show Instruction where
    show (Instruction l c v) =
        "\nInstruction { line = " ++ show l ++
        ", command = " ++ show c ++
        ", value = " ++ show v ++
        " }"

instructions :: [Instruction]
instructions = [
    Instruction {line = 0, command = "push", value = Just (AstInteger 0)},
    Instruction {line = 1, command = "get", value = Just (AstSymbol "x")},
    Instruction {line = 2, command = "call", value = Just (AstSymbol "eq?")},
    Instruction {line = 3, command = "jumpIfFalse", value = Just (AstInteger 7)},
    Instruction {line = 4, command = "push", value = Just (AstInteger 1)},
    Instruction {line = 5, command = "return", value = Nothing},
    Instruction {line = 6, command = "get", value = Just (AstSymbol "foo")},
    Instruction {line = 7, command = "return", value = Nothing}
    ]

stack :: [Stack]
stack = [
    AstInteger 10,
    AstInteger 2
    ]

jump :: [Instruction] -> Int -> [Instruction]
jump [] _ = []
jump (x:xs) lineNum
    | line x == lineNum = x:xs
    | otherwise = jump xs lineNum


modulo :: Stack
modulo (a:b:[]) env = case eval a env of
    (Value a1) -> case eval b env of
        (Value 0) -> (Err ("Divide by zero in function modulo " ++ show a ++ " " ++ show b))
        (Value a2) -> (Val (a1`mod`a2))
        (Error err) -> (Err err)
        (_) -> (Err ("Invalid arguments to function mod " ++ show a ++ " " ++ show b))
    (Error err) -> (Err err)
    (_) -> (Err ("Invalid arguments to function mod " ++ show a ++ " " ++ show b))
modulo _ _ = (Err "Invalid arguments to function mod")




toto :: IO ()
-- toto = print instructions
-- toto = print (jump instructions 3)
toto = print (modulo stack)