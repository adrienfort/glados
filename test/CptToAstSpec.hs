module CptToAstSpec (spec) where

import Test.Hspec
import CptToAst

cptToAstBasicSpec :: Spec
cptToAstBasicSpec = do
  describe "CptToAst Basic" $ do
    it "case Integer" $ do
      cptToAst (CptInteger 1) `shouldBe` AstInteger 1
    it "case #t" $ do
      cptToAst (CptSymbols "#t") `shouldBe` AstBoolean "#t"
    it "case #f" $ do
      cptToAst (CptSymbols "#f") `shouldBe` AstBoolean "#f"
    it "case a" $ do
      cptToAst (CptSymbols "a") `shouldBe` AstSymbol "a"

cptToAstDefineSpec :: Spec
cptToAstDefineSpec = do
  describe "CptToAst define" $ do
    it "case define x 1" $ do
      cptToAst (CptLists[CptSymbols "define", CptSymbols "x", CptInteger 1])
        `shouldBe` AstDefine (Left "x") (AstInteger 1)
    it "case define x 'hello'" $ do
      cptToAst (CptLists[CptSymbols "define", CptSymbols "x", CptSymbols "hello"])
        `shouldBe` AstDefine (Left "x") (AstSymbol "hello")
    it "case define x #f" $ do
      cptToAst (CptLists[CptSymbols "define", CptSymbols "x", CptSymbols "#f"])
        `shouldBe` AstDefine (Left "x") (AstBoolean "#f")
    it "case define (x) 12" $ do
      cptToAst (CptLists[CptSymbols "define", CptLists[CptSymbols "a"], CptInteger 12])
        `shouldBe` AstDefine (Right ["a"]) (AstInteger 12)
    it "case define (a) 'world'" $ do
      cptToAst (CptLists[CptSymbols "define", CptLists[CptSymbols "a"], CptSymbols "world"])
        `shouldBe` AstDefine (Right ["a"]) (AstSymbol "world")
    it "case define (a) #t" $ do
      cptToAst (CptLists[CptSymbols "define", CptLists[CptSymbols "a"], CptSymbols "#t"])
        `shouldBe` AstDefine (Right ["a"]) (AstBoolean "#t")

cptToAstCallSpec :: Spec
cptToAstCallSpec = do
  describe "CptToAst Call" $ do
    it "case define (x) (1)" $ do
      cptToAst (CptLists[CptSymbols "define", CptLists[CptSymbols "x"], CptLists[CptInteger 1]])
        `shouldBe` AstDefine (Right ["x"]) (AstCall [AstInteger 1])
    it "case define (x) ('hello')" $ do
      cptToAst (CptLists[CptSymbols "define", CptLists[CptSymbols "x"], CptLists[CptSymbols "hello"]])
        `shouldBe` AstDefine (Right ["x"]) (AstCall [AstSymbol "hello"])
    it "case define (x) (#f)" $ do
      cptToAst (CptLists[CptSymbols "define", CptLists[CptSymbols "x"], CptLists[CptSymbols "#f"]])
        `shouldBe` AstDefine (Right ["x"]) (AstCall [AstBoolean "#f"])
    it "case define (a b c) (+ b c)" $ do
      cptToAst (CptLists[CptSymbols "define", CptLists[CptSymbols "a", CptSymbols "b", CptSymbols "c"], CptLists[CptSymbols "+", CptSymbols "b", CptSymbols "c"]])
        `shouldBe` AstDefine (Right ["a", "b", "c"]) (AstCall [AstSymbol "+", AstSymbol "b", AstSymbol "c"])
    it "case define (x a b) (eq? a b)" $ do
      cptToAst (CptLists[CptSymbols "define", CptLists[CptSymbols "x", CptSymbols "a", CptSymbols "b"], CptLists[CptSymbols "eq?", CptSymbols "a", CptSymbols "b"]])
        `shouldBe` AstDefine (Right ["x", "a", "b"]) (AstCall [AstSymbol "eq?", AstSymbol "a", AstSymbol "b"])
    it "case (a)" $ do
      cptToAst (CptLists[CptSymbols "a"])
        `shouldBe` AstCall [AstSymbol "a"]
    it "case (1)" $ do
      cptToAst (CptLists[CptInteger 1])
        `shouldBe` AstCall [AstInteger 1]
    it "case ('hello')" $ do
      cptToAst (CptLists[CptSymbols "hello"])
        `shouldBe` AstCall [AstSymbol "hello"]
    it "case (#t)" $ do
      cptToAst (CptLists[CptSymbols "#t"])
        `shouldBe` AstCall [AstBoolean "#t"]
    it "case (a 11 22)" $ do
      cptToAst (CptLists[CptSymbols "a", CptInteger 11, CptInteger 12])
        `shouldBe` AstCall [AstSymbol "a", AstInteger 11, AstInteger 12]
    it "case (repeat 2 'hello world')" $ do
      cptToAst (CptLists[CptSymbols "repeat", CptInteger 2, CptSymbols "hello world"])
        `shouldBe` AstCall [AstSymbol "repeat", AstInteger 2, AstSymbol "hello world"]
    it "case (concat 'hello' 'world')" $ do
      cptToAst (CptLists[CptSymbols "concat", CptSymbols "hello", CptSymbols "world"])
        `shouldBe` AstCall [AstSymbol "concat", AstSymbol "hello", AstSymbol "world"]
    it "case (or #t #t)" $ do
      cptToAst (CptLists[CptSymbols "or", CptSymbols "#t", CptSymbols "#t"])
        `shouldBe` AstCall [AstSymbol "or", AstBoolean "#t", AstBoolean "#t"]

cptToAstLambdaSpec :: Spec
cptToAstLambdaSpec = do
  describe "CptToAst Lambda" $ do
    it "case (lambda (a) (- a))" $ do
      cptToAst (CptLists[CptSymbols "lambda", CptLists[CptSymbols "a"], CptLists[CptSymbols "-", CptSymbols "a"]])
        `shouldBe` (AstLambda ["a"] (AstCall [AstSymbol "-", AstSymbol "a"]))
    it "case ((lambda (a b) (+ a b)) 'hello' #t)" $ do
      cptToAst (CptLists[CptLists[CptSymbols "lambda", CptLists[CptSymbols "a", CptSymbols "b"], CptLists[CptSymbols "+", CptSymbols "a", CptSymbols "b"]], CptSymbols "hello", CptSymbols "#t"])
        `shouldBe` (AstCall [(AstLambda ["a", "b"] (AstCall [AstSymbol "+", AstSymbol "a", AstSymbol "b"])), AstSymbol "hello", AstBoolean "#t"])
    it "case define x (lambda (a b) (- a b))" $ do
      cptToAst (CptLists[CptSymbols "define", CptSymbols "x", CptLists[CptSymbols "lambda", CptLists[CptSymbols "a", CptSymbols "b"], CptLists[CptSymbols "-", CptSymbols "a", CptSymbols "b"]]])
        `shouldBe` AstDefine (Left "x") (AstLambda ["a", "b"] (AstCall [AstSymbol "-", AstSymbol "a", AstSymbol "b"]))
    it "case define x ((lambda (a b) (+ a b)) 1 2)" $ do
      cptToAst (CptLists[CptSymbols "define", CptSymbols "x", CptLists[CptLists[CptSymbols "lambda", CptLists[CptSymbols "a", CptSymbols "b"], CptLists[CptSymbols "+", CptSymbols "a", CptSymbols "b"]], CptInteger 1, CptInteger 2]])
        `shouldBe` AstDefine (Left "x") (AstCall [(AstLambda ["a", "b"] (AstCall [AstSymbol "+", AstSymbol "a", AstSymbol "b"])), AstInteger 1, AstInteger 2])

cptToAstGlobalSpec :: Spec
cptToAstGlobalSpec = do
  describe "CptToAst Global" $ do
    it "case (Define add (lambda (a b) (+ a b))) (add 3 4)" $ do
      startCptToAst (CptLists [CptLists [CptSymbols "define", CptSymbols "add", CptLists [CptSymbols "lambda", CptLists [CptSymbols "a", CptSymbols "b"], CptLists [CptSymbols "+", CptSymbols "a", CptSymbols "b"]]], CptLists [CptSymbols "add", CptInteger 3, CptInteger 4]])
        `shouldBe` [AstDefine (Left "add") (AstLambda ["a", "b"] (AstCall [AstSymbol "+", AstSymbol "a", AstSymbol "b"])), AstCall [AstSymbol "add", AstInteger 3, AstInteger 4]]
    it "(define (fact x) (if (eq? x 1) 1 (* x (fact (- x 1))))) (fact 10)" $ do
      startCptToAst (CptLists [CptLists [CptSymbols "define", CptLists [CptSymbols "fact", CptSymbols "x"], CptLists [CptSymbols "if", CptLists [CptSymbols "eq?", CptSymbols "x", CptInteger 1], CptInteger 1, CptLists [CptSymbols "*", CptSymbols "x", CptLists [CptSymbols "fact", CptLists [CptSymbols "-", CptSymbols "x", CptInteger 1]]]]], CptLists [CptSymbols "fact", CptInteger 10]])
        `shouldBe` [AstDefine (Right ["fact", "x"]) (AstCall [AstSymbol "if", AstCall [AstSymbol "eq?", AstSymbol "x", AstInteger 1], AstInteger 1, AstCall [AstSymbol "*", AstSymbol "x", AstCall [AstSymbol "fact", AstCall [AstSymbol "-", AstSymbol "x", AstInteger 1]]]]), AstCall [AstSymbol "fact", AstInteger 10]]

spec :: Spec
spec = do
  cptToAstBasicSpec
  cptToAstDefineSpec
  cptToAstCallSpec
  cptToAstLambdaSpec
  cptToAstGlobalSpec