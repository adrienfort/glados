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
                    ], 0, [
                        ("x", Right (AstInteger 1))
                    ])

compileSimpleExpressionSpec :: Spec
compileSimpleExpressionSpec = do
    describe "simple expression" $ do
        it "(define (x a b) 2) (x 1 2)" $ do
            compile [AstDefine (Right ["x", "a", "b"]) (AstInteger 2), AstCall [AstSymbol "x", AstInteger 1, AstInteger 2]] 0 [] `shouldBe` (Left [
                Instruction {line = 0, command = "push", value = Just (AstInteger 1)},
                Instruction {line = 1, command = "push", value = Just (AstInteger 2)},
                Instruction {line = 2, command = "call", value = Just (AstSymbol "x")},
                Instruction {line = 3, command = "return", value = Nothing}
                ], 4, [
                    ("x", Left (["a", "b"], [
                        Instruction {line = 0, command = "push", value = Just (AstInteger 2)},
                        Instruction {line = 1, command = "return", value = Nothing}
                    ]))
                ])
        it "(define (x a b) 2) (x 1 2 3)" $ do
            compile [AstDefine (Right ["x", "a", "b"]) (AstInteger 2), AstCall [AstSymbol "x", AstInteger 1, AstInteger 2, AstInteger 3]] 0 [] `shouldBe` (Right "x invalid call"
                , 0, [
                    ("x", Left (["a", "b"], [
                        Instruction {line = 0, command = "push", value = Just (AstInteger 2)},
                        Instruction {line = 1, command = "return", value = Nothing}
                    ]))
                ])
        it "(lambda (x) 2)" $ do
            compile [AstLambda ["x"] (AstInteger 2)] 0 [] `shouldBe` (Left [
                Instruction {line = 0, command = "push", value = Just (AstInteger 2)},
                Instruction {line = 1, command = "call", value = Just (AstSymbol "lambda")},
                Instruction {line = 2, command = "deleteEnv", value = Just (AstSymbol "lambda")},
                Instruction {line = 3, command = "return", value = Nothing}
                ], 4, [
                    ("lambda", Left (["x"],
                    [Instruction {line = 0, command = "push", value = Just (AstInteger 2)}]))
                ])
        it "(define x 5)" $ do
            compile [AstDefine (Left "x") (AstInteger 5)] 0 [] `shouldBe` (Left [
                ], 0, [("x", Right (AstInteger 5))])

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
                    ("fact", Left (["x"],
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
                    ("fact", Left (["x"],
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

compileTrickySpec :: Spec
compileTrickySpec = do
    describe "tricky test" $ do
        it "1 (+ 2)" $ do
            compile [AstInteger 1, AstCall [AstSymbol "+", AstInteger 2]] 0 [] `shouldBe` (Right "+ invalid call", 2, [])

spec :: Spec
spec = do
    compileIfSpec
    compileSimpleExpressionSpec
    compileHardSpec
    compileTrickySpec