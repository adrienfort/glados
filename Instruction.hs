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

type Env = (String, Either ([String], [Instruction]) Ast)

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




-- add :: Stack -> Env -> Stack
-- add stack env = stack

-- eval :: [Instruction] -> Env -> Stack -> ([Instruction], Stack)
-- eval 

    -- (Left newstack) -> exec b env newstack
    -- (Right error) -> Right error





toto :: IO ()
-- toto = print instructions
toto = print (jump instructions 3)
-- toto = print (modulo stack)