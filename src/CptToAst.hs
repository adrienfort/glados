module CptToAst (
  Cpt (..),
  Ast (..),
  printCpt,
  printAst,
  cptToAst
) where

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

data Ast = AstInteger Int
  | AstSymbol String
  | AstBoolean String
  | AstCall [Ast]
  | AstDefine (Either String [String]) Ast
  | AstLambda [String] Ast

instance Show Ast where
    show (AstInteger n) = show n
    show (AstSymbol n) = n
    show (AstBoolean n) = "<boolean>" ++ n
    show (AstCall []) = "<call>"
    show (AstCall (s:_)) = "<call>" ++ show s
    show (AstDefine (Left s) n) = "<var>" ++ s ++ " " ++ show n
    show (AstDefine (Right []) n) = "<func> " ++ show n
    show (AstDefine (Right (s:_)) n) = "<func>" ++ s ++ " " ++ show n
    show (AstLambda [] n) = "<lambda>" ++ show n
    show (AstLambda (s:_) n) = "<lambda>" ++ s ++ " " ++ show n

instance Eq Ast where
    (AstInteger n1) == (AstInteger n2) = n1 == n2
    (AstSymbol n1) == (AstSymbol n2) = n1 == n2
    (AstBoolean n1) == (AstBoolean n2) = n1 == n2
    (AstCall c1) == (AstCall c2) = c1 == c2
    (AstDefine (Left s1) n1) == (AstDefine (Left s2) n2) = s1 == s2 && n1 == n2
    (AstDefine (Right s1) n1) == (AstDefine (Right s2) n2) = s1 == s2 && n1 == n2
    (AstLambda s1 n1) == (AstLambda s2 n2) = s1 == s2 && n1 == n2
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