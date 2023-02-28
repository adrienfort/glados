module EvalSpec (spec) where

import Test.Hspec
import Eval
import Lib

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

-- TestPush :: Spec
-- TestPush = do
    -- describe "test success" $ do
        -- it "(x 1 2)" $ do
            -- compile [AstCall [AstSymbol "x", AstInteger 1, AstInteger 2]] 0 [] `shouldBe` (Left [
            --     Instruction {line = 0, command = "push", value = Just (AstInteger 1)},
            --     Instruction {line = 1, command = "push", value = Just (AstInteger 2)},
            --     Instruction {line = 2, command = "call", value = Just (AstSymbol "x")},
            --     Instruction {line = 3, command = "return", value = Nothing}
            --     ], 4, [])
            -- exec (AstInteger 10) -> 

spec :: Spec
spec = do
    toto