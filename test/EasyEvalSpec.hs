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
    describe "Lambdas" $ do
        it "(define add (lambda () 12)) (add)" $ do
            let env = eval (Define (Left "add") (Lambda [] (Integer 12))) []
            eval (Call [Symbol "add"]) (getEnv env) `shouldBe` (Value 12)
        it "(define add (lambda () 12)) add" $ do
            let env = eval (Define (Left "add") (Lambda [] (Integer 12))) []
            eval (Symbol "add") (getEnv env) `shouldBe` (Expression "function add")
        it "(define add (lambda (a b) (+ a b))) (add 3 4)" $ do
            let env = eval (Define (Left "add") (Lambda ["a", "b"] (Call [Symbol "+", Symbol "a", Symbol "b"]))) []
            eval (Call [Symbol "add", Integer 3, Integer 4]) (getEnv env) `shouldBe` (Value 7)
    describe "Advanced functions" $ do
        it "(define (> a b) (if (eq? a b) #f (if (< a b) #f #t))) (> 10 -2)" $ do
            let env = eval (Define (Right [">", "a", "b"]) (Call [Symbol "if", Call [Symbol "eq?", Symbol "a", Symbol "b"], Boolean "#f", Call [Symbol "if", Call [Symbol "<", Symbol "a", Symbol "b"], Boolean "#f", Boolean "#t"]])) []
            eval (Call [Symbol ">", Integer 10, Integer (-2)]) (getEnv env) `shouldBe` (Bolean "#t")
    describe "Recursive functions" $ do
        it "(define (fact x) (if (eq? x 1) 1 (* x (fact (- x 1))))) (fact 10)" $ do
            let env = eval (Define (Right ["fact", "x"]) (Call [Symbol "if", Call [Symbol "eq?", Symbol "x", Integer 1], Integer 1, Call [Symbol "*", Symbol "x", Call [Symbol "fact", Call [Symbol "-", Symbol "x", Integer 1]]]])) []
            eval (Call [Symbol "fact", Integer 10]) (getEnv env) `shouldBe` (Value 3628800)