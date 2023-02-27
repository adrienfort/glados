module CompileSpec (spec) where

import Test.Hspec
import Compile
import Lib

-- compile [] 0 [] `shouldBe` (Left [
-- ], 0, [])

compileIfSpec :: Spec
compileIfSpec = do
    describe "simple if" $ do
        it "(if #t 1 0)" $ do
            compile [(AstCall [AstSymbol "if", AstBoolean "#t", AstInteger 1, AstInteger 0])] 0 [] `shouldBe` (Left [
                    Instruction {line = 0, command = "push", value = Just (AstBoolean "#t")},
                    Instruction {line = 1, command = "jumpIfFalse", value = Just (AstInteger 4)},
                    Instruction {line = 2, command = "push", value = Just (AstInteger 1)},
                    Instruction {line = 3, command = "return", value = Nothing},
                    Instruction {line = 4, command = "push", value = Just (AstInteger 0)},
                    Instruction {line = 5, command = "return", value = Nothing}
                    ], 6, [])
        it "(define x (if #t 1 0))" $ do
            compile [(AstDefine (Left "x") (AstCall [AstSymbol "if", AstBoolean "#t", AstInteger 1, AstInteger 0]))] 0 [] `shouldBe` (Left [
                    Instruction {line = 0, command = "push", value = Just (AstBoolean "#t")},
                    Instruction {line = 1, command = "jumpIfFalse", value = Just (AstInteger 3)},
                    Instruction {line = 2, command = "push", value = Just (AstInteger 1)},
                    Instruction {line = 3, command = "push", value = Just (AstInteger 0)},
                    Instruction {line = 4, command = "define", value = Just (AstSymbol "x")},
                    Instruction {line = 5, command = "return", value = Nothing}
                    ], 6, [])

compileSimpleExpressionSpec :: Spec
compileSimpleExpressionSpec = do
    describe "simple expression" $ do
        it "(x 1 2)" $ do
            compile [AstCall [AstSymbol "x", AstInteger 1, AstInteger 2]] 0 [] `shouldBe` (Left [
                Instruction {line = 0, command = "push", value = Just (AstInteger 1)},
                Instruction {line = 1, command = "push", value = Just (AstInteger 2)},
                Instruction {line = 2, command = "call", value = Just (AstSymbol "x")},
                Instruction {line = 3, command = "return", value = Nothing}
                ], 4, [])
        it "(x 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16)" $ do
            compile [AstCall [AstSymbol "x", AstInteger 1, AstInteger 2, AstInteger 3, AstInteger 4, AstInteger 5, AstInteger 6, AstInteger 7, AstInteger 8, AstInteger 9, AstInteger 10, AstInteger 11, AstInteger 12, AstInteger 13, AstInteger 14, AstInteger 15, AstInteger 16]] 0 [] `shouldBe` (Left [
                Instruction {line = 0, command = "push", value = Just (AstInteger 1)},
                Instruction {line = 1, command = "push", value = Just (AstInteger 2)},
                Instruction {line = 2, command = "push", value = Just (AstInteger 3)},
                Instruction {line = 3, command = "push", value = Just (AstInteger 4)},
                Instruction {line = 4, command = "push", value = Just (AstInteger 5)},
                Instruction {line = 5, command = "push", value = Just (AstInteger 6)},
                Instruction {line = 6, command = "push", value = Just (AstInteger 7)},
                Instruction {line = 7, command = "push", value = Just (AstInteger 8)},
                Instruction {line = 8, command = "push", value = Just (AstInteger 9)},
                Instruction {line = 9, command = "push", value = Just (AstInteger 10)},
                Instruction {line = 10, command = "push", value = Just (AstInteger 11)},
                Instruction {line = 11, command = "push", value = Just (AstInteger 12)},
                Instruction {line = 12, command = "push", value = Just (AstInteger 13)},
                Instruction {line = 13, command = "push", value = Just (AstInteger 14)},
                Instruction {line = 14, command = "push", value = Just (AstInteger 15)},
                Instruction {line = 15, command = "push", value = Just (AstInteger 16)},
                Instruction {line = 16, command = "call", value = Just (AstSymbol "x")},
                Instruction {line = 17, command = "return", value = Nothing}
                ], 18, [])
        it "(lambda (x) 2)" $ do
            compile [AstLambda ["x"] (AstInteger 2)] 0 [] `shouldBe` (Left [
                Instruction {line = 0, command = "push", value = Just (AstInteger 2)},
                Instruction {line = 1, command = "call", value = Just (AstSymbol "lambda")},
                Instruction {line = 2, command = "deleteEnv", value = Just (AstSymbol "lambda")},
                Instruction {line = 3, command = "return", value = Nothing}
                ], 4, [
                    ("lambda", (["x"],
                    [Instruction {line = 0, command = "push", value = Just (AstInteger 2)}]))
                ])
        it "(define x 5)" $ do
            compile [AstDefine (Left "x") (AstInteger 5)] 0 [] `shouldBe` (Left [
                Instruction {line = 0, command = "push", value = Just (AstInteger 5)},
                Instruction {line = 1, command = "define", value = Just (AstSymbol "x")},
                Instruction {line = 2, command = "return", value = Nothing}
                ], 3, [])

compileHardSpec :: Spec
compileHardSpec = do
    describe "hard test" $ do
        it "(define fact (lambda (x) (if (eq? x 1) 1 (* x (fact (- x 1)))))) (fact 10)" $ do
            compile [
                AstDefine (Left "fact") (AstLambda ["x"] (AstCall [AstSymbol "if", AstCall [AstSymbol "eq?", AstSymbol "x", AstInteger 1], AstInteger 1, AstCall [AstSymbol "*", AstSymbol "x", AstCall [AstSymbol "fact", AstCall [AstSymbol "-", AstSymbol "x", AstInteger 1]]]])),
                AstCall [AstSymbol "fact", AstInteger 10]] 0 []
                `shouldBe` (Left [
                Instruction {line = 0, command = "push", value = Just (AstInteger 10)},
                Instruction {line = 1, command = "call", value = Just (AstSymbol "fact")},
                Instruction {line = 2, command = "return", value = Nothing}
                ], 3, [
                    ("fact", (["x"],
                    [
                        Instruction {line = 0, command = "get", value = Just (AstSymbol "x")},
                        Instruction {line = 1, command = "push", value = Just (AstInteger 1)},
                        Instruction {line = 2, command = "call", value = Just (AstSymbol "eq?")},
                        Instruction {line = 3, command = "jumpIfFalse", value = Just (AstInteger 6)},
                        Instruction {line = 4, command = "push", value = Just (AstInteger 1)},
                        Instruction {line = 5, command = "return", value = Nothing},
                        Instruction {line = 6, command = "get", value = Just (AstSymbol "x")}, -- pop *
                        Instruction {line = 7, command = "get", value = Just (AstSymbol "x")}, -- pop -
                        Instruction {line = 8, command = "push", value = Just (AstInteger 1)}, -- pop -
                        Instruction {line = 9, command = "call", value = Just (AstSymbol "-")}, -- pop 2 | pop fact
                        Instruction {line = 10, command = "call", value = Just (AstSymbol "fact")}, -- pop 1 | pop *
                        Instruction {line = 11, command = "call", value = Just (AstSymbol "*")}, -- pop 2
                        Instruction {line = 12, command = "return", value = Nothing}
                    ])
                )])
        it "(define (fact x) (if (eq? x 1) 1 (* x (fact (- x 1))))) (fact 10)" $ do
            compile [AstDefine (Right ["fact", "x"]) (AstCall [AstSymbol "if", AstCall [AstSymbol "eq?", AstSymbol "x", AstInteger 1], AstInteger 1, AstCall [AstSymbol "*", AstSymbol "x", AstCall [AstSymbol "fact", AstCall [AstSymbol "-", AstSymbol "x", AstInteger 1]]]]), AstCall [AstSymbol "fact", AstInteger 10]] 0 [] `shouldBe` (Left [
                Instruction {line = 0, command = "push", value = Just (AstInteger 10)},
                Instruction {line = 1, command = "call", value = Just (AstSymbol "fact")},
                Instruction {line = 2, command = "return", value = Nothing}
                ], 3, [
                    ("fact", (["x"],
                    [
                        Instruction {line = 0, command = "get", value = Just (AstSymbol "x")},
                        Instruction {line = 1, command = "push", value = Just (AstInteger 1)},
                        Instruction {line = 2, command = "call", value = Just (AstSymbol "eq?")},
                        Instruction {line = 3, command = "jumpIfFalse", value = Just (AstInteger 6)},
                        Instruction {line = 4, command = "push", value = Just (AstInteger 1)},
                        Instruction {line = 5, command = "return", value = Nothing},
                        Instruction {line = 6, command = "get", value = Just (AstSymbol "x")}, -- pop *
                        Instruction {line = 7, command = "get", value = Just (AstSymbol "x")}, -- pop -
                        Instruction {line = 8, command = "push", value = Just (AstInteger 1)}, -- pop -
                        Instruction {line = 9, command = "call", value = Just (AstSymbol "-")}, -- pop 2 | pop fact
                        Instruction {line = 10, command = "call", value = Just (AstSymbol "fact")}, -- pop 1 | pop *
                        Instruction {line = 11, command = "call", value = Just (AstSymbol "*")}, -- pop 2
                        Instruction {line = 12, command = "return", value = Nothing}
                    ])
                )])

spec :: Spec
spec = do
    compileIfSpec
    compileSimpleExpressionSpec
    compileHardSpec