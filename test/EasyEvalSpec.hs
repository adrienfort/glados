module EasyEvalSpec (spec) where

import Test.Hspec
import Eval.Eval

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
        it "(define x (define a 2))" $ do
            isResultError (eval (Define (Left "x") (Define (Left "a") (Integer 2))) []) `shouldBe` True
        it "(define (x) (define a 2) (x))" $ do
            let env = eval (Define (Right ["x"]) (Define (Left "a") (Integer 2))) []
            isResultError (eval (Call [Symbol "x"]) (getEnv env)) `shouldBe` True
    describe "Builtins call" $ do
        it "(* 2 3)" $ do
            eval (Call [Symbol "*", Integer 2, Integer 3]) [] `shouldBe` (Value 6)
        it "(+ 2 3)" $ do
            eval (Call [Symbol "+", Integer 2, Integer 3]) [] `shouldBe` (Value 5)
        it "(- 2 3)" $ do
            eval (Call [Symbol "-", Integer 2, Integer 3]) [] `shouldBe` (Value (-1))
        it "(div 6 3)" $ do
            eval (Call [Symbol "div", Integer 6, Integer 3]) [] `shouldBe` (Value 2)
        it "(mod 3 3)" $ do
            eval (Call [Symbol "mod", Integer 3, Integer 3]) [] `shouldBe` (Value 0)
        it "(eq? 3 3)" $ do
            eval (Call [Symbol "eq?", Integer 3, Integer 3]) [] `shouldBe` (Bolean "#t")
        it "(eq? 2 3)" $ do
            eval (Call [Symbol "eq?", Integer 2, Integer 3]) [] `shouldBe` (Bolean "#f")
    describe "Builtins error handling" $ do
        it "(* 2)" $ do
            isResultError (eval (Call [Symbol "*", Integer 2]) []) `shouldBe` True
        it "(+ 2)" $ do
            isResultError (eval (Call [Symbol "+", Integer 2]) []) `shouldBe` True
        it "(- 2)" $ do
            isResultError (eval (Call [Symbol "-", Integer 2]) []) `shouldBe` True
        it "(div 6)" $ do
            isResultError (eval (Call [Symbol "div", Integer 6]) []) `shouldBe` True
        it "(mod 3)" $ do
            isResultError (eval (Call [Symbol "mod", Integer 3]) []) `shouldBe` True
        it "(eq? 3)" $ do
            isResultError (eval (Call [Symbol "eq?", Integer 3]) []) `shouldBe` True
        it "(eq? 2)" $ do
            isResultError (eval (Call [Symbol "eq?", Integer 2]) []) `shouldBe` True
    describe "If function" $ do
        it "(if #t 1 2)" $ do
            eval (Call [Symbol "if", Boolean "#t", Integer 1, Integer 2]) [] `shouldBe` (Value 1)
        it "(if #f 1 2)" $ do
            eval (Call [Symbol "if", Boolean "#f", Integer 1, Integer 2]) [] `shouldBe` (Value 2)
        it "(if (eq? 2 2) 1 2)" $ do
            eval (Call [Symbol "if", Call [Symbol "eq?", Integer 2, Integer 2], Integer 1, Integer 2]) [] `shouldBe` (Value 1)
        it "(if (eq? 1 2) 1 2)" $ do
            eval (Call [Symbol "if", Call [Symbol "eq?", Integer 1, Integer 2], Integer 1, Integer 2]) [] `shouldBe` (Value 2)
    describe "If error handling" $ do
        it "(if #t 1 )" $ do
            isResultError (eval (Call [Symbol "if", Boolean "#t", Integer 1]) []) `shouldBe` True
        it "(if 1 1 2)" $ do
            isResultError (eval (Call [Symbol "if", Boolean "#t", Integer 1]) []) `shouldBe` True
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
    describe "Advanced lambdas functions" $ do
        it "(define > (lambda (a b) (if (eq? a b) #f (if (< a b) #f #t))) (> 10 -2)" $ do
            let env = eval (Define (Left ">") (Lambda ["a", "b"] (Call [Symbol "if", Call [Symbol "eq?", Symbol "a", Symbol "b"], Boolean "#f", Call [Symbol "if", Call [Symbol "<", Symbol "a", Symbol "b"], Boolean "#f", Boolean "#t"]]))) []
            eval (Call [Symbol ">", Integer 10, Integer (-2)]) (getEnv env) `shouldBe` (Bolean "#t")
    describe "Recursive functions" $ do
        it "(define (fact x) (if (eq? x 1) 1 (* x (fact (- x 1))))) (fact 10)" $ do
            let env = eval (Define (Right ["fact", "x"]) (Call [Symbol "if", Call [Symbol "eq?", Symbol "x", Integer 1], Integer 1, Call [Symbol "*", Symbol "x", Call [Symbol "fact", Call [Symbol "-", Symbol "x", Integer 1]]]])) []
            eval (Call [Symbol "fact", Integer 10]) (getEnv env) `shouldBe` (Value 3628800)
    describe "Recursive lambdas" $ do
        it "(define fact (lambda (x) (if (eq? x 1) 1 (* x (fact (- x 1)))))) (fact 10)" $ do
            let env = eval (Define (Left "fact") (Lambda ["x"] (Call [Symbol "if", Call [Symbol "eq?", Symbol "x", Integer 1], Integer 1, Call [Symbol "*", Symbol "x", Call [Symbol "fact", Call [Symbol "-", Symbol "x", Integer 1]]]]))) []
            eval (Call [Symbol "fact", Integer 10]) (getEnv env) `shouldBe` (Value 3628800)
    describe "Error handling lambda" $ do
        it "((lambda (x) (if (eq? x 1) 1 (* x (fact (- x 1)))))) 10)" $ do
            eval (Call [Lambda ["x"] (Call [Symbol "if", Call [Symbol "eq?", Symbol "x", Integer 1], Integer 1, Call [Symbol "*", Symbol "x", Call [Symbol "fact", Call [Symbol "-", Symbol "x", Integer 1]]]]), Integer 10]) [] `shouldBe` (Error "Bad call")
