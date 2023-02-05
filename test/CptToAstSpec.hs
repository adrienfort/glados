module CptToAstSpec (cptToAstSpec) where

import Test.Hspec
import CptToAst

cptToAstBasicSpec :: IO ()
cptToAstBasicSpec = hspec $ do
  describe "CptToAst Basic" $ do
    it "case Integer" $ do
      cptToAst (CptInteger 1) `shouldBe` [AstInteger 1]
    it "case True" $ do
      cptToAst (CptSymbols "True") `shouldBe` [AstBoolean "True"]
    it "case False" $ do
      cptToAst (CptSymbols "False") `shouldBe` [AstBoolean "False"]
    it "case a" $ do
      cptToAst (CptSymbols "a") `shouldBe` [AstSymbol "a"]

cptToAstDefineSpec :: IO ()
cptToAstDefineSpec = hspec $ do
  describe "CptToAst Define" $ do
    it "case Define x 1" $ do
      cptToAst (CptLists[CptSymbols "Define", CptSymbols "x", CptInteger 1])
        `shouldBe` [AstDefine (Left "x") (AstInteger 1)]
    it "case Define x 'hello'" $ do
      cptToAst (CptLists[CptSymbols "Define", CptSymbols "x", CptSymbols "hello"])
        `shouldBe` [AstDefine (Left "x") (AstSymbol "hello")]
    it "case Define x False" $ do
      cptToAst (CptLists[CptSymbols "Define", CptSymbols "x", CptSymbols "False"])
        `shouldBe` [AstDefine (Left "x") (AstBoolean "False")]
    it "case Define (x) 12" $ do
      cptToAst (CptLists[CptSymbols "Define", CptLists[CptSymbols "a"], CptInteger 12])
        `shouldBe` [AstDefine (Right ["a"]) (AstInteger 12)]
    it "case Define (a) 'world'" $ do
      cptToAst (CptLists[CptSymbols "Define", CptLists[CptSymbols "a"], CptSymbols "world"])
        `shouldBe` [AstDefine (Right ["a"]) (AstSymbol "world")]
    it "case Define (a) True" $ do
      cptToAst (CptLists[CptSymbols "Define", CptLists[CptSymbols "a"], CptSymbols "True"])
        `shouldBe` [AstDefine (Right ["a"]) (AstBoolean "True")]

cptToAstCallSpec :: IO ()
cptToAstCallSpec = hspec $ do
  describe "CptToAst Call" $ do
    it "case Define (x) (1)" $ do
      cptToAst (CptLists[CptSymbols "Define", CptLists[CptSymbols "x"], CptLists[CptInteger 1]])
        `shouldBe` [AstDefine (Right ["x"]) (AstCall [AstInteger 1])]
    it "case Define (x) ('hello')" $ do
      cptToAst (CptLists[CptSymbols "Define", CptLists[CptSymbols "x"], CptLists[CptSymbols "hello"]])
        `shouldBe` [AstDefine (Right ["x"]) (AstCall [AstSymbol "hello"])]
    it "case Define (x) (False)" $ do
      cptToAst (CptLists[CptSymbols "Define", CptLists[CptSymbols "x"], CptLists[CptSymbols "False"]])
        `shouldBe` [AstDefine (Right ["x"]) (AstCall [AstBoolean "False"])]
    it "case Define (a b c) (+ b c)" $ do
      cptToAst (CptLists[CptSymbols "Define", CptLists[CptSymbols "a", CptSymbols "b", CptSymbols "c"], CptLists[CptSymbols "+", CptSymbols "b", CptSymbols "c"]])
        `shouldBe` [AstDefine (Right ["a", "b", "c"]) (AstCall [AstSymbol "+", AstSymbol "b", AstSymbol "c"])]
    it "case Define (x a b) (eq? a b)" $ do
      cptToAst (CptLists[CptSymbols "Define", CptLists[CptSymbols "x", CptSymbols "a", CptSymbols "b"], CptLists[CptSymbols "eq?", CptSymbols "a", CptSymbols "b"]])
        `shouldBe` [AstDefine (Right ["x", "a", "b"]) (AstCall [AstSymbol "eq?", AstSymbol "a", AstSymbol "b"])]
    it "case (a)" $ do
      cptToAst (CptLists[CptSymbols "a"])
        `shouldBe` [AstCall [AstSymbol "a"]]
    it "case (1)" $ do
      cptToAst (CptLists[CptInteger 1])
        `shouldBe` [AstCall [AstInteger 1]]
    it "case ('hello')" $ do
      cptToAst (CptLists[CptSymbols "hello"])
        `shouldBe` [AstCall [AstSymbol "hello"]]
    it "case (True)" $ do
      cptToAst (CptLists[CptSymbols "True"])
        `shouldBe` [AstCall [AstBoolean "True"]]
    it "case (a 11 22)" $ do
      cptToAst (CptLists[CptSymbols "a", CptInteger 11, CptInteger 12])
        `shouldBe` [AstCall [AstSymbol "a", AstInteger 11, AstInteger 12]]
    it "case (repeat 2 'hello world')" $ do
      cptToAst (CptLists[CptSymbols "repeat", CptInteger 2, CptSymbols "hello world"])
        `shouldBe` [AstCall [AstSymbol "repeat", AstInteger 2, AstSymbol "hello world"]]
    it "case (concat 'hello' 'world')" $ do
      cptToAst (CptLists[CptSymbols "concat", CptSymbols "hello", CptSymbols "world"])
        `shouldBe` [AstCall [AstSymbol "concat", AstSymbol "hello", AstSymbol "world"]]
    it "case (or True True)" $ do
      cptToAst (CptLists[CptSymbols "or", CptSymbols "True", CptSymbols "True"])
        `shouldBe` [AstCall [AstSymbol "or", AstBoolean "True", AstBoolean "True"]]

cptToAstLambdaSpec :: IO ()
cptToAstLambdaSpec = hspec $ do
  describe "CptToAst Lambda" $ do
    it "case (lambda (a) (- a))" $ do
      cptToAst (CptLists[CptSymbols "lambda", CptLists[CptSymbols "a"], CptLists[CptSymbols "-", CptSymbols "a"]])
        `shouldBe` [(AstLambda ["a"] (AstCall [AstSymbol "-", AstSymbol "a"]))]
    it "case ((lambda (a b) (+ a b)) 'hello' True)" $ do
      cptToAst (CptLists[CptLists[CptSymbols "lambda", CptLists[CptSymbols "a", CptSymbols "b"], CptLists[CptSymbols "+", CptSymbols "a", CptSymbols "b"]], CptSymbols "hello", CptSymbols "True"])
        `shouldBe` [(AstCall [(AstLambda ["a", "b"] (AstCall [AstSymbol "+", AstSymbol "a", AstSymbol "b"])), AstSymbol "hello", AstBoolean "True"])]
    it "case Define x (lambda (a b) (- a b))" $ do
      cptToAst (CptLists[CptSymbols "Define", CptSymbols "x", CptLists[CptSymbols "lambda", CptLists[CptSymbols "a", CptSymbols "b"], CptLists[CptSymbols "-", CptSymbols "a", CptSymbols "b"]]])
        `shouldBe` [AstDefine (Left "x") (AstLambda ["a", "b"] (AstCall [AstSymbol "-", AstSymbol "a", AstSymbol "b"]))]
    it "case Define x ((lambda (a b) (+ a b)) 1 2)" $ do
      cptToAst (CptLists[CptSymbols "Define", CptSymbols "x", CptLists[CptLists[CptSymbols "lambda", CptLists[CptSymbols "a", CptSymbols "b"], CptLists[CptSymbols "+", CptSymbols "a", CptSymbols "b"]], CptInteger 1, CptInteger 2]])
        `shouldBe` [AstDefine (Left "x") (AstCall [(AstLambda ["a", "b"] (AstCall [AstSymbol "+", AstSymbol "a", AstSymbol "b"])), AstInteger 1, AstInteger 2])]

cptToAstSpec :: IO ()
cptToAstSpec = do
  cptToAstBasicSpec
  cptToAstDefineSpec
  cptToAstCallSpec
  cptToAstLambdaSpec