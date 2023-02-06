module EasyEvalSpec (spec) where

import Test.Hspec
import Eval.Eval
import Lib

isResultError :: Result -> Bool
isResultError (Error _) = True
isResultError _ = False

spec :: Spec
spec = do
    describe "Simple single evaluation" $ do
        it "(define x 2)" $ do
            eval (AstDefine (Left "x") (AstInteger 2)) [] `shouldBe` (Environment [("x", (AstInteger 2))])
        it "(define x #t)" $ do
            eval (AstDefine (Left "x") (AstBoolean "#t")) [] `shouldBe` (Environment [("x", (AstBoolean "#t"))])
        it "2" $ do
            eval (AstInteger 2) [] `shouldBe` (Value 2)
        it "#t" $ do
            eval (AstBoolean "#t") [] `shouldBe` (Bolean "#t")
    describe "Simple define Astsymbol" $ do
        it "(define x 2) x" $ do
            let env = eval (AstDefine (Left "x") (AstInteger 2)) []
            eval (AstSymbol "x") (getEnv env) `shouldBe` (Value 2)
        it "(define x #t) x" $ do
            let env = eval (AstDefine (Left "x") (AstBoolean "#t")) []
            eval (AstSymbol "x") (getEnv env) `shouldBe` (Bolean "#t")
    describe "Simple define Astsymbol error handling" $ do
        it "x" $ do
            isResultError(eval (AstSymbol "x") []) `shouldBe` True
    describe "Simple define Function" $ do
        it "(define (x) 2) x" $ do
            let env = eval (AstDefine (Right ["x"]) (AstInteger 2)) []
            eval (AstSymbol "x") (getEnv env) `shouldBe` (Expression "function x")
        it "(define (x) 2) (x)" $ do
            let env = eval (AstDefine (Right ["x"]) (AstInteger 2)) []
            eval (AstCall [AstSymbol "x"]) (getEnv env) `shouldBe` (Value 2)
    describe "Simple define Function error handling" $ do
        it "(define () 2) x " $ do
            isResultError (eval (AstDefine (Right []) (AstInteger 2)) []) `shouldBe` True
        it "(define x (define a 2))" $ do
            isResultError (eval (AstDefine (Left "x") (AstDefine (Left "a") (AstInteger 2))) []) `shouldBe` True
        it "(define (x) (define a 2) (x))" $ do
            let env = eval (AstDefine (Right ["x"]) (AstDefine (Left "a") (AstInteger 2))) []
            isResultError (eval (AstCall [AstSymbol "x"]) (getEnv env)) `shouldBe` True



    describe "Builtins Astcall" $ do
        it "(* 2 3)" $ do
            eval (AstCall [AstSymbol "*", AstInteger 2, AstInteger 3]) [] `shouldBe` (Value 6)
        it "(* 3 2)" $ do
            eval (AstCall [AstSymbol "*", AstInteger 3, AstInteger 2]) [] `shouldBe` (Value 6)

        it "(* -2 3)" $ do
            eval (AstCall [AstSymbol "*", AstInteger (-2), AstInteger 3]) [] `shouldBe` (Value (-6))
        it "(* -3 2)" $ do
            eval (AstCall [AstSymbol "*", AstInteger 2, AstInteger (-3)]) [] `shouldBe` (Value (-6))
        it "(* 3 -2)" $ do
            eval (AstCall [AstSymbol "*", AstInteger 3, AstInteger (-2)]) [] `shouldBe` (Value (-6))
        it "(* 2 -3)" $ do
            eval (AstCall [AstSymbol "*", AstInteger 2, AstInteger (-3)]) [] `shouldBe` (Value (-6))

        it "(* -2 -3)" $ do
            eval (AstCall [AstSymbol "*", AstInteger (-2), AstInteger (-3)]) [] `shouldBe` (Value 6)
        it "(* -3 -2)" $ do
            eval (AstCall [AstSymbol "*", AstInteger (-3), AstInteger (-2)]) [] `shouldBe` (Value 6)



        it "(+ 2 3)" $ do
            eval (AstCall [AstSymbol "+", AstInteger 2, AstInteger 3]) [] `shouldBe` (Value 5)
        it "(+ -2 3)" $ do
            eval (AstCall [AstSymbol "+", AstInteger (-2), AstInteger 3]) [] `shouldBe` (Value 1)
        it "(+ 2 -3)" $ do
            eval (AstCall [AstSymbol "+", AstInteger 2, AstInteger (-3)]) [] `shouldBe` (Value (-1))


        it "(- 2 3)" $ do
            eval (AstCall [AstSymbol "-", AstInteger 2, AstInteger 3]) [] `shouldBe` (Value (-1))
        it "(div 6 3)" $ do
            eval (AstCall [AstSymbol "div", AstInteger 6, AstInteger 3]) [] `shouldBe` (Value 2)
        it "(mod 3 3)" $ do
            eval (AstCall [AstSymbol "mod", AstInteger 3, AstInteger 3]) [] `shouldBe` (Value 0)
        it "(eq? 3 3)" $ do
            eval (AstCall [AstSymbol "eq?", AstInteger 3, AstInteger 3]) [] `shouldBe` (Bolean "#t")
        it "(eq? 2 3)" $ do
            eval (AstCall [AstSymbol "eq?", AstInteger 2, AstInteger 3]) [] `shouldBe` (Bolean "#f")
    describe "Builtins error handling" $ do
        it "(* 2)" $ do
            isResultError (eval (AstCall [AstSymbol "*", AstInteger 2]) []) `shouldBe` True
        it "(+ 2)" $ do
            isResultError (eval (AstCall [AstSymbol "+", AstInteger 2]) []) `shouldBe` True
        it "(- 2)" $ do
            isResultError (eval (AstCall [AstSymbol "-", AstInteger 2]) []) `shouldBe` True
        it "(div 6)" $ do
            isResultError (eval (AstCall [AstSymbol "div", AstInteger 6]) []) `shouldBe` True
        it "(mod 3)" $ do
            isResultError (eval (AstCall [AstSymbol "mod", AstInteger 3]) []) `shouldBe` True
        it "(eq? 3)" $ do
            isResultError (eval (AstCall [AstSymbol "eq?", AstInteger 3]) []) `shouldBe` True
        it "(eq? 2)" $ do
            isResultError (eval (AstCall [AstSymbol "eq?", AstInteger 2]) []) `shouldBe` True
    describe "If function" $ do
        it "(if #t 1 2)" $ do
            eval (AstCall [AstSymbol "if", AstBoolean "#t", AstInteger 1, AstInteger 2]) [] `shouldBe` (Value 1)
        it "(if #f 1 2)" $ do
            eval (AstCall [AstSymbol "if", AstBoolean "#f", AstInteger 1, AstInteger 2]) [] `shouldBe` (Value 2)
        it "(if (eq? 2 2) 1 2)" $ do
            eval (AstCall [AstSymbol "if", AstCall [AstSymbol "eq?", AstInteger 2, AstInteger 2], AstInteger 1, AstInteger 2]) [] `shouldBe` (Value 1)
        it "(if (eq? 1 2) 1 2)" $ do
            eval (AstCall [AstSymbol "if", AstCall [AstSymbol "eq?", AstInteger 1, AstInteger 2], AstInteger 1, AstInteger 2]) [] `shouldBe` (Value 2)
    describe "If error handling" $ do
        it "(if #t 1 )" $ do
            isResultError (eval (AstCall [AstSymbol "if", AstBoolean "#t", AstInteger 1]) []) `shouldBe` True
        it "(if 1 1 2)" $ do
            isResultError (eval (AstCall [AstSymbol "if", AstBoolean "#t", AstInteger 1]) []) `shouldBe` True
    describe "Lambdas" $ do
        it "(define add (lambda () 12)) (add)" $ do
            let env = eval (AstDefine (Left "add") (AstLambda [] (AstInteger 12))) []
            eval (AstCall [AstSymbol "add"]) (getEnv env) `shouldBe` (Value 12)
        it "(define add (lambda () 12)) add" $ do
            let env = eval (AstDefine (Left "add") (AstLambda [] (AstInteger 12))) []
            eval (AstSymbol "add") (getEnv env) `shouldBe` (Expression "function add")
        it "(define add (lambda (a b) (+ a b))) (add 3 4)" $ do
            let env = eval (AstDefine (Left "add") (AstLambda ["a", "b"] (AstCall [AstSymbol "+", AstSymbol "a", AstSymbol "b"]))) []
            eval (AstCall [AstSymbol "add", AstInteger 3, AstInteger 4]) (getEnv env) `shouldBe` (Value 7)
    describe "Advanced functions" $ do
        it "(define (> a b) (if (eq? a b) #f (if (< a b) #f #t))) (> 10 -2)" $ do
            let env = eval (AstDefine (Right [">", "a", "b"]) (AstCall [AstSymbol "if", AstCall [AstSymbol "eq?", AstSymbol "a", AstSymbol "b"], AstBoolean "#f", AstCall [AstSymbol "if", AstCall [AstSymbol "<", AstSymbol "a", AstSymbol "b"], AstBoolean "#f", AstBoolean "#t"]])) []
            eval (AstCall [AstSymbol ">", AstInteger 10, AstInteger (-2)]) (getEnv env) `shouldBe` (Bolean "#t")
    describe "Advanced lambdas functions" $ do
        it "(define > (lambda (a b) (if (eq? a b) #f (if (< a b) #f #t))) (> 10 -2)" $ do
            let env = eval (AstDefine (Left ">") (AstLambda ["a", "b"] (AstCall [AstSymbol "if", AstCall [AstSymbol "eq?", AstSymbol "a", AstSymbol "b"], AstBoolean "#f", AstCall [AstSymbol "if", AstCall [AstSymbol "<", AstSymbol "a", AstSymbol "b"], AstBoolean "#f", AstBoolean "#t"]]))) []
            eval (AstCall [AstSymbol ">", AstInteger 10, AstInteger (-2)]) (getEnv env) `shouldBe` (Bolean "#t")
    describe "Recursive functions" $ do
        it "(define (fact x) (if (eq? x 1) 1 (* x (fact (- x 1))))) (fact 10)" $ do
            let env = eval (AstDefine (Right ["fact", "x"]) (AstCall [AstSymbol "if", AstCall [AstSymbol "eq?", AstSymbol "x", AstInteger 1], AstInteger 1, AstCall [AstSymbol "*", AstSymbol "x", AstCall [AstSymbol "fact", AstCall [AstSymbol "-", AstSymbol "x", AstInteger 1]]]])) []
            eval (AstCall [AstSymbol "fact", AstInteger 10]) (getEnv env) `shouldBe` (Value 3628800)
    describe "Recursive lambdas" $ do
        it "(define fact (lambda (x) (if (eq? x 1) 1 (* x (fact (- x 1)))))) (fact 10)" $ do
            let env = eval (AstDefine (Left "fact") (AstLambda ["x"] (AstCall [AstSymbol "if", AstCall [AstSymbol "eq?", AstSymbol "x", AstInteger 1], AstInteger 1, AstCall [AstSymbol "*", AstSymbol "x", AstCall [AstSymbol "fact", AstCall [AstSymbol "-", AstSymbol "x", AstInteger 1]]]]))) []
            eval (AstCall [AstSymbol "fact", AstInteger 10]) (getEnv env) `shouldBe` (Value 3628800)
    describe "Error handling lambda" $ do
        it "((lambda (x) (if (eq? x 1) 1 (* x (fact (- x 1)))))) 10)" $ do
            eval (AstCall [AstLambda ["x"] (AstCall [AstSymbol "if", AstCall [AstSymbol "eq?", AstSymbol "x", AstInteger 1], AstInteger 1, AstCall [AstSymbol "*", AstSymbol "x", AstCall [AstSymbol "fact", AstCall [AstSymbol "-", AstSymbol "x", AstInteger 1]]]]), AstInteger 10]) [] `shouldBe` (Error "Bad call")
