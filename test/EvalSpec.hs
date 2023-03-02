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
            ifcondition [AstBoolean "#f", AstInteger 1] `shouldBe` (Right "Error in the size of stack in ifcondition")

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
            add [AstInteger (1)] `shouldBe` (Right "Error in the size of stack in add")

    describe "minus with Stack" $ do
        it "- 1 2" $ do
            minus [AstInteger 2, AstInteger 1] `shouldBe` (Left [AstInteger (-1)])
        it "- 1 -2" $ do
            minus [AstInteger (-2), AstInteger 1] `shouldBe` (Left [AstInteger 3])
        it "- -1 2" $ do
            minus [AstInteger 2, AstInteger (-1)] `shouldBe` (Left [AstInteger (-3)])
        it "- -1 -2" $ do
            minus [AstInteger (-2), AstInteger (-1)] `shouldBe` (Left [AstInteger 1])
        it "- 1" $ do
            minus [AstInteger (1)] `shouldBe` (Right "Error in the size of stack in minus")

    describe "mult with Stack" $ do
        it "* 1 2" $ do
            mult [AstInteger 2, AstInteger 1] `shouldBe` (Left [AstInteger 2])
        it "* 1 -2" $ do
            mult [AstInteger (-2), AstInteger 1] `shouldBe` (Left [AstInteger (-2)])
        it "* -1 2" $ do
            mult [AstInteger 2, AstInteger (-1)] `shouldBe` (Left [AstInteger (-2)])
        it "* -1 -2" $ do
            mult [AstInteger (-2), AstInteger (-1)] `shouldBe` (Left [AstInteger 2])
        it "* 1" $ do
            mult [AstInteger (1)] `shouldBe` (Right "Error in the size of stack in mult")

    describe "division with Stack" $ do
        it "/ 3 1" $ do
            division [AstInteger 1, AstInteger 3] `shouldBe` (Left [AstInteger 3])
        it "/ 1 2" $ do -- I-m not sure but rounded down
            division [AstInteger 2, AstInteger 1] `shouldBe` (Left [AstInteger 0])
        it "/ 3 2" $ do -- I-m not sure but rounded down
            division [AstInteger 2, AstInteger 3] `shouldBe` (Left [AstInteger 1])
        it "/ 2 3" $ do -- I-m not sure but rounded down
            division [AstInteger 3, AstInteger 2] `shouldBe` (Left [AstInteger 0])
        it "/ 8 10" $ do -- I-m not sure but rounded down
            division [AstInteger 10, AstInteger 8] `shouldBe` (Left [AstInteger 0])
        it "/ 0 2" $ do
            division [AstInteger 2, AstInteger 0] `shouldBe` (Left [AstInteger 0])
        it "/ 2 0" $ do
            division [AstInteger 0, AstInteger 2] `shouldBe` (Right "Divide by zero in function division")
        it "/ 1" $ do
            division [AstInteger (1)] `shouldBe` (Right "Error in the size of stack in division")

    describe "modulo with Stack" $ do
        it "% 6 4" $ do
            modulo [AstInteger 4, AstInteger 6] `shouldBe` (Left [AstInteger 2])
        it "% 0 2" $ do
            modulo [AstInteger 2, AstInteger 0] `shouldBe` (Left [AstInteger 0])
        it "% 2 0" $ do
            modulo [AstInteger 0, AstInteger 2] `shouldBe` (Right "Divide by zero in function modulo")
        it "% 1" $ do
            modulo [AstInteger (1)] `shouldBe` (Right "Error in the size of stack in modulo")
    

spec :: Spec
spec = do
    pushSpec