module Eval.Lib
    (
        Ast (..),
        Env
    ) where

data Ast = Integer Int | Symbol String | Boolean String | Call [Ast] | Define (Either String [String]) Ast | Lambda [String] Ast

type Env = [(String, Ast)]

instance Show Ast where
    show (Integer n) = show n
    show (Symbol n) = n
    show (Boolean n) = n
    show (Call []) = "<function>"
    show (Call (s:_)) = "<function>" ++ show s
    show (Define (Left s) n) = s ++ " " ++ show n
    show (Define (Right []) n) = show n
    show (Define (Right (s:_)) n) = s ++ " " ++ show n
    show (Lambda [] n) = show n
    show (Lambda (s:_) n) = s ++ " " ++ show n

instance Eq Ast where
    (Integer n1) == (Integer n2) = n1 == n2
    (Symbol n1) == (Symbol n2) = n1 == n2
    (Boolean n1) == (Boolean n2) = n1 == n2
    (Call s1) == (Call s2) = s1 == s2
    (Define (Left s1) n1) == (Define (Left s2) n2) = s1 == s2 && n1 == n2
    (Define (Right s1) n1) == (Define (Right s2) n2) = s1 == s2 && n1 == n2
    (Lambda s1 n1) == (Lambda s2 n2) = s1 == s2 && n1 == n2
    _ == _ = False
