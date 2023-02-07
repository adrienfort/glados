module CptToAst (
  Cpt (..),
  Ast (..),
  printCpt,
  printAst,
  cptToAst
) where

import Lib

data Cpt = CptLists [Cpt]
  | CptSymbols String
  | CptInteger Int

instance Show Cpt where
  show (CptLists (a : b)) = show a ++ show b
  show (CptLists []) = ""
  show (CptSymbols s) = show s
  show (CptInteger i) = show i

instance Eq Cpt where
  (CptLists (a1 : b1)) == (CptLists (a2 : b2)) = a1 == a2 && b1 == b2
  (CptLists []) == (CptLists []) = True
  (CptSymbols s1) == (CptSymbols s2) = s1 == s2
  (CptInteger i1) == (CptInteger i2) = i1 == i2
  _ == _ = False

printCpt :: Cpt -> IO ()
printCpt cpt = putStrLn (show cpt)

printAst :: Ast -> IO ()
printAst ast = putStrLn (show ast)

cptToAst :: Cpt -> [Ast]
cptToAst cpt@(CptLists (CptLists ((CptSymbols "lambda"): _) : _)) = [cptToAstLine cpt]
cptToAst (CptLists [CptLists a]) = [cptToAstLine (CptLists a)]
cptToAst (CptLists (CptLists a : b)) = [cptToAstLine (CptLists a)] ++ cptToAst (CptLists b)
cptToAst cpt = [cptToAstLine cpt]

cptToAstLine :: Cpt -> Ast
cptToAstLine (CptInteger i) = AstInteger i
cptToAstLine (CptSymbols "#t") = AstBoolean "#t"
cptToAstLine (CptSymbols "#f") = AstBoolean "#f"
cptToAstLine (CptSymbols s) = AstSymbol s
cptToAstLine (CptLists [CptSymbols "define", CptSymbols s, value]) = AstDefine (Left s) (cptToAstLine value)
cptToAstLine (CptLists [CptSymbols "define", CptLists keys, value]) = AstDefine (Right (cptListSymbolsToStringArray keys)) (cptToAstLine value)
cptToAstLine (CptLists [CptSymbols "lambda", CptLists args, value]) = AstLambda (cptListSymbolsToStringArray args) (cptToAstLine value)
cptToAstLine (CptLists l) = AstCall (cptListToAst l)

cptListSymbolsToStringArray :: [Cpt] -> [String]
cptListSymbolsToStringArray (CptSymbols a : b) = [a] ++ cptListSymbolsToStringArray b
cptListSymbolsToStringArray [] = []
cptListSymbolsToStringArray _ = []

cptListToAst :: [Cpt] -> [Ast]
cptListToAst (a : b) = [cptToAstLine a] ++ cptListToAst b
cptListToAst [] = []