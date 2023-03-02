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

-- exec :: [Instruction] -> Env -> Stack -> Either Ast String
-- type Env = [(String, Either ([String], [Instruction]) Ast)]

pushSpec :: Spec
pushSpec = do
    describe "test success" $ do
        it "(1)" $ do
            exec [
                Instruction {line = 0, command = "push", value = Just (AstInteger 1)},
                Instruction {line = 1, command = "return", value = Nothing}
                ] [
                    ("sucess", Left (["x"], [])),
                    ("sucess", Right (AstInteger 2))
                ] [] `shouldBe` (Left (AstInteger 1))

    describe "ifcondition with Stack" $ do
        it "= 1 1" $ do
            ifcondition [AstInteger 1, AstInteger 1] `shouldBe` (Left [AstBoolean "#t"])
        it "= 1 2" $ do
            ifcondition [AstInteger 2, AstInteger 1] `shouldBe` (Left [AstBoolean "#f"])
        it "= #t #t" $ do
            ifcondition [AstBoolean "#t", AstBoolean "#t"] `shouldBe` (Left [AstBoolean "#t"])
        it "= #t #f" $ do
            ifcondition [AstBoolean "#f", AstBoolean "#t"] `shouldBe` (Left [AstBoolean "#f"])
        it "= 1 #f" $ do
            ifcondition [AstBoolean "#f", AstInteger 1] `shouldBe` (Right "Error in the stack of ifcondition")

    describe "add with Stack" $ do
        it "+ 1 2" $ do
            add [AstInteger 2, AstInteger 1] `shouldBe` (Left [AstInteger 3])
        it "+ 1 -2" $ do
            add [AstInteger (-2), AstInteger 1] `shouldBe` (Left [AstInteger (-1)])
        it "+ -1 2" $ do
            add [AstInteger 2, AstInteger (-1)] `shouldBe` (Left [AstInteger 1])
        it "+ -1 -2" $ do
            add [AstInteger (-2), AstInteger (-1)] `shouldBe` (Left [AstInteger (-3)])
        it "+ 1" $ do
            add [AstInteger (1)] `shouldBe` (Right "Error in the stack of add")
    

spec :: Spec
spec = do
    pushSpec