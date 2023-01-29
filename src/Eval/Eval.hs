module Eval.Eval
    (
        Ast,
        Env,
        eval
    ) where

-- Define env
-- Comment on fait pour les variables temporaires

data Ast = Integer Int | Symbol String | Boolean Bool | Call String [Ast] | Define [String] Ast
type Env = [(String, Ast)]
type Function = [Ast] -> Env -> Either Int String

getKeyValue :: String -> Env -> Ast
getKeyValue _ _ = (Integer 1)

addKeyValue :: String -> Ast -> Env -> Env
addKeyValue _ _ env = env

removeKeyValue :: String -> Env -> Env
removeKeyValue _ env = env

getBuiltins :: [(String, Function)]
getBuiltins = []

-- Can return
--  left : result
--  right "no" : didn't found
--  right : error
isBuiltin :: Ast -> Env -> Either Int String
isBuiltin (Call a b) env = case lookup a getBuiltins of
                    Nothing -> Right "no"
                    Just bu -> bu b env
isBuiltin _ _ = Right "Bad call"

callFunc :: Ast -> Env -> Either Int String
callFunc a env = Left 1 -- check env


eval :: Ast -> Env -> Either Int String
eval (Define a b) _ = 1
eval (Call a b) _ = 2
eval _ _ = 3