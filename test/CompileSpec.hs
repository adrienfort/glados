module CompileSpec (spec) where

import Test.Hspec
import Compile
import Lib

compileIfSpec :: Spec
compileIfSpec = do
    describe "simple if" $ do
        -- 0 1 2 3 4 5
        it "(if #t 1 0)" $ do
            compile [(AstCall [AstSymbol "if", AstBoolean "#t", AstInteger 1, AstInteger 0])] 0 `shouldBe` (Left [
                    Instruction {line = 0, command = "push", value = Just (AstBoolean "#t")},
                    Instruction {line = 1, command = "jumpIfFalse", value = Just (AstInteger 4)},
                    Instruction {line = 2, command = "push", value = Just (AstInteger 1)},
                    Instruction {line = 3, command = "return", value = Nothing},
                    Instruction {line = 4, command = "push", value = Just (AstInteger 0)},
                    Instruction {line = 5, command = "return", value = Nothing}
                    ])

-- compileSimpleExpression = do

spec :: Spec
spec = do
    compileIfSpec