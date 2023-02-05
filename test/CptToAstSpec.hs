module CptToAstSpec (cptToAstSpec) where

import Test.Hspec
import CptToAst

cptToAstSpec :: IO ()
cptToAstSpec = hspec $ do
  describe "CptToAst" $ do
    it "case Integer" $ do
      cptToAst (CptInteger 1) `shouldBe` [AstInteger 1]
    it "case Symbol 'True'" $ do
      cptToAst (CptSymbols "True") `shouldBe` [AstBoolean "True"]
    it "case Symbol 'False'" $ do
      cptToAst (CptSymbols "False") `shouldBe` [AstBoolean "False"]
    it "case other Symbol" $ do
      cptToAst (CptSymbols "+") `shouldBe` [AstSymbol "+"]
    it "case List(Symbol 'Define', Symbol 'x', Integer 1)" $ do
      cptToAst (CptLists[CptSymbols "Define", CptSymbols "x", CptInteger 1])
        `shouldBe` [AstDefine (Left "x") (AstInteger 1)]
    it "case List(Symbol 'Define', Symbol 'x', Symbol 'hello')" $ do
      cptToAst (CptLists[CptSymbols "Define", CptSymbols "x", CptSymbols "hello"])
        `shouldBe` [AstDefine (Left "x") (AstSymbol "hello")]
    it "case List(Symbol 'Define', List(Symbol 'x'), List(Integer 1))" $ do
      cptToAst (CptLists[CptSymbols "Define", CptLists[CptSymbols "x"], CptLists[CptInteger 1]])
        `shouldBe` [AstDefine (Right ["x"]) (AstInteger 1)]
    it "case List(Symbol 'Define', List(Symbol 'x'), List(Symbol 'hello'))" $ do
      cptToAst (CptLists[CptSymbols "Define", CptLists[CptSymbols "x"], CptLists[CptSymbols "hello"]])
        `shouldBe` [AstDefine (Right ["x"]) (AstSymbol "hello")]