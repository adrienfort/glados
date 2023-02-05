module EasyEvalSpec (spec) where

import Test.Hspec
import Eval.Eval

-- data Result = 
    -- Value Int | 
    -- Environment Env | 
    -- Bolean String | 
    -- Expression String | 
    -- Error String
isResultError :: Result -> Bool
isResultError (Error _) = True
isResultError _ = False

spec :: Spec
spec = do
    describe "Simple single evaluation" $ do
        it "(define x 2)" $ do
            eval (Define (Left "x") (Integer 2)) [] `shouldBe` (Environment [("x", (Integer 2))])
        it "(define x #t)" $ do
            eval (Define (Left "x") (Boolean "#t")) [] `shouldBe` (Environment [("x", (Boolean "#t"))])
        it "2" $ do
            eval (Integer 2) [] `shouldBe` (Value 2)
        it "#t" $ do
            eval (Boolean "#t") [] `shouldBe` (Bolean "#t")
    describe "Simple define symbol" $ do
        it "(define x 2) x" $ do
            let env = eval (Define (Left "x") (Integer 2)) []
            eval (Symbol "x") (getEnv env) `shouldBe` (Value 2)
        it "(define x #t) x" $ do
            let env = eval (Define (Left "x") (Boolean "#t")) []
            eval (Symbol "x") (getEnv env) `shouldBe` (Bolean "#t")
    describe "Simple define symbol error handling" $ do
        it "x" $ do
            isResultError(eval (Symbol "x") []) `shouldBe` True
    describe "Simple define Function" $ do
        it "(define (x) 2) x" $ do
            let env = eval (Define (Right ["x"]) (Integer 2)) []
            eval (Symbol "x") (getEnv env) `shouldBe` (Expression "function x")
        it "(define (x) 2) (x)" $ do
            let env = eval (Define (Right ["x"]) (Integer 2)) []
            eval (Call [Symbol "x"]) (getEnv env) `shouldBe` (Value 2)
    describe "Simple define Function error handling" $ do
        it "(define () 2) x " $ do
            isResultError (eval (Define (Right []) (Integer 2)) []) `shouldBe` True