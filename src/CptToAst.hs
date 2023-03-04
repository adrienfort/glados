module CptToAst (
  Cpt (..),
  Ast (..),
  printCpt,
  printAst,
  cptToAst
) where

import Lib

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

-- 
-- transitionCptToAst :: [Cpt] -> [Ast]
-- transitionCptToAst [] = []
-- transitionCptToAst (a:b) = cptToAst a : transitionCptToAst b
-- 
-- startCptToAst :: Cpt -> [Ast]
-- startCptToAst (CptLists (CptLists a : b)) = transitionCptToAst (CptLists a: b)
-- startCptToAst a = [cptToAst a]
-- 
-- cptToAst :: Cpt -> Ast
-- cptToAst (CptInteger i) = AstInteger i
-- cptToAst (CptSymbols "#t") = AstBoolean "#t"
-- cptToAst (CptSymbols "#f") = AstBoolean "#f"
-- cptToAst (CptSymbols s) = AstSymbol s
-- cptToAst (CptLists [CptSymbols "define", CptSymbols s, value]) = AstDefine (Left s) (cptToAst value)
-- cptToAst (CptLists [CptSymbols "define", CptLists keys, value]) = AstDefine (Right (cptListSymbolsToStringArray keys)) (cptToAst value)
-- cptToAst (CptLists [CptSymbols "lambda", CptLists args, value]) = AstLambda (cptListSymbolsToStringArray args) (cptToAst value)
-- cptToAst (CptLists l) = AstCall (cptListToAst l)
-- 
-- cptListSymbolsToStringArray :: [Cpt] -> [String]
-- cptListSymbolsToStringArray (CptSymbols a : b) = [a] ++ cptListSymbolsToStringArray b
-- cptListSymbolsToStringArray [] = []
-- cptListSymbolsToStringArray _ = []
-- 
-- cptListToAst :: [Cpt] -> [Ast]
-- cptListToAst (a : b) = [cptToAst a] ++ cptListToAst b
-- cptListToAst [] = []