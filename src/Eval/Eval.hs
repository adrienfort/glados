module Eval.Eval
    (
        Ast (..),
        Env,
        Result (..),
        eval
    ) where

data Ast = Integer Int | Symbol String | Boolean String | Call String [Ast] | Define (Either String [String]) Ast | Lambda [String] Ast
-- data Ast = Integer { getIntAst :: Int }
    -- | Symbol { getsymbolAst :: String }
    -- | Boolean { getboolAst :: String }
    -- | Call { getCallNameAst :: String, getCallArgsAst :: [Ast] }
    -- | Define { getDefineHeaderAst :: (Either String [String]), getDefineBodyAst :: Ast }
    -- | Lambda { getLambdaHeaderAst :: (Either String [String]), getLambdaBodyAst :: Ast }

instance Show Ast where
    show (Integer n) = show n
    show (Symbol n) = n
    show (Boolean n) = n
    show (Call s _) = "<function>" ++ s
    show (Define (Left s) n) = s ++ " " ++ show n
    show (Define (Right []) n) = show n
    show (Define (Right (s:_)) n) = s ++ " " ++ show n
    show (Lambda [] n) = show n
    show (Lambda (s:_) n) = s ++ " " ++ show n

instance Eq Ast where
    (Integer n1) == (Integer n2) = n1 == n2
    (Symbol n1) == (Symbol n2) = n1 == n2
    (Boolean n1) == (Boolean n2) = n1 == n2
    (Call s1 n1) == (Call s2 n2) = s1 == s2 && n1 == n2
    (Define (Left s1) n1) == (Define (Left s2) n2) = s1 == s2 && n1 == n2
    (Define (Right s1) n1) == (Define (Right s2) n2) = s1 == s2 && n1 == n2
    (Lambda s1 n1) == (Lambda s2 n2) = s1 == s2 && n1 == n2
    _ == _ = False

type Env = [(String, Ast)]

data ReturnValue = Val Int | Bool String | Err String
type Function = [Ast] -> Env -> ReturnValue
data Result = Value Int | Environment { getEnv :: Env }| Bolean String | Expression String | Error String

instance Eq Result where
    (Environment n1) == (Environment n2) = n1 == n2
    (Value n1) == (Value n2) = n1 == n2
    (Bolean n1) == (Bolean n2) = n1 == n2
    (Expression n1) == (Expression n2) = n1 == n2
    (Error n1) == (Error n2) = n1 == n2
    _ == _ = False

instance Show Result where
    show (Value n) = show n
    show (Environment n) = show n
    show (Bolean n) = n
    show (Expression n) = n
    show (Error n) = n
getKeyValue :: String -> Env -> Either Ast String
getKeyValue str env = case lookup str env of
        Nothing -> Right (str ++ " is undefined")
        Just val -> Left val


-- add a key value and can replace an existing key value
insertKeyValue :: String -> Ast -> Env -> Env
insertKeyValue str ast env = case lookup str env of
    Nothing -> (str, ast) : env
    Just _ -> replaceKey env
        where
            replaceKey [] = []
            replaceKey ((s, val) : b) | s == str = ((str, ast) : b)
                                    | otherwise = ((s, val) : (replaceKey b))

-- add a new key value
addKeyValue :: String -> Ast -> Env -> Env
addKeyValue str ast env = (str, ast) : env

getBuiltins :: [(String, Function)]
getBuiltins = []

isBuiltin :: Ast -> Env -> ReturnValue
isBuiltin (Call a b) env = case lookup a getBuiltins of
                    Nothing -> Bool "no"
                    Just bu -> bu b env
isBuiltin _ _ = Err "Bad call"

-- and a function arguments into the env
setFunctionEnv :: [String] -> [Ast] -> Env -> Either Env String
setFunctionEnv [] (_:_) _ = Right "dog"
setFunctionEnv (_:_) [] _ = Right "dog"
setFunctionEnv [] [] env = Left env
setFunctionEnv (s:sr) (b:br) env = case (eval b env) of
    (Value v) -> setFunctionEnv sr br (addKeyValue s (Integer v) env)
    (Bolean v) -> setFunctionEnv sr br (addKeyValue s (Boolean v) env)
    (Error err) -> Right err
    _ -> Right ("Func " ++ s ++ ": no expression in body")

callFunc :: Ast -> Env -> ReturnValue
-- call lambda
callFunc (Call a []) env = case getKeyValue a env of -- call without args
    Right err -> (Err err)
    Left (Define (Right s) e) -> case (length s) /= 0 of 
        False -> case eval e env of -- function doesn't need args
            (Value v) -> (Val v)
            (Bolean v) -> (Bool v)
            (Error err) -> (Err err)
            _ -> (Err ("Bad function call : " ++ a))
        True -> (Err ("Calling " ++ a ++ " with incorrect number of arguments")) -- func need args
    Left _ -> (Err ("Invalid call " ++ a)) -- get args
callFunc (Call a b) env = case getKeyValue a env of
    Right err -> (Err err)
    Left (Define (Right s) e) -> case (length s) == (length b) of -- check args nbr
        False -> (Err ("Calling " ++ a ++ " with incorrect number of arguments"))
        True -> case setFunctionEnv s b env of -- insert evaluated args in env
            Left nenv -> case eval e nenv of
                (Value v) -> (Val v)
                (Bolean v) -> (Bool v)
                (Error err) -> (Err err)
                _ -> (Err ("Bad function call : " ++ a))
            Right err -> (Err err)
    Left _ -> (Err ("Invalid call " ++ a)) -- get args
callFunc _ _ = (Err "Invalid syntax")

functionValue :: String -> [Ast] -> Env -> Result
functionValue s e env = case isBuiltin (Call s e) env of
    (Val a) -> (Value a) -- check builtin
    (Bool "no") -> case callFunc (Call s e) env of
        (Val a) -> (Value a)
        (Bool a) -> (Bolean a)
        (Err err) -> (Error err)
    (Bool a) -> (Bolean a)
    (Err err) -> (Error err)


getSymbol :: String -> Env -> Result
getSymbol str env = case getKeyValue str env of
    Right err -> (Error err) -- error : didn't find
    Left (Integer a) -> (Value a)
    Left (Boolean a) -> (Bolean a)
    Left (Symbol a) -> eval (Symbol a) env
    Left (Define _ _) -> (Expression ("function " ++ str))
    Left (Lambda _ _) -> (Expression ("function " ++ str))
    Left _ -> (Error "Unknown value")

-- PROBLEME :

-- (define x (lambda (a b) (+ a b))) = assignation de fonction
-- => reconnaitre lambda
-- -> mettre un type lambda
-- (define y ((lambda (a b) (+ a b)) 1 2) ) = result appel de fonction
-- => reconnaitre appel à une lambda
-- -> Call _ lambda

defineSymbol :: Either String [String] -> Ast -> Env -> Result
defineSymbol (Left a) body env = case body of -- syntax (define x body)
        (Define _ _) -> (Error ("Symbol " ++ a ++ " invalid assignation"))
        _ -> case eval body env of
            (Environment _) -> (Error ("Symbol " ++ a ++ " invalid assignation (null type)"))
            (Error err) -> (Error err)
            (Value val) -> (Environment (insertKeyValue a (Integer val) env)) -- get a int value from either an int, a symbol or a function call
            (Bolean val) -> (Environment (insertKeyValue a (Boolean val) env)) -- get a boolean value from either a boolean, a symbol or a function call
            (Expression _) -> (Environment (insertKeyValue a body env)) -- got an expression as return value => either a function or a lambda
defineSymbol (Right []) _ _ = (Error "Symbol name is not defined")
defineSymbol (Right (a:b)) body env = (Environment (insertKeyValue a (Define (Right b) body) env))

eval :: Ast -> Env -> Result
eval (Integer a) _ = (Value a)
eval (Boolean a) _ = (Bolean a)
eval (Symbol a) env = getSymbol a env
eval (Lambda _ _) _ = (Expression "lambda")
eval (Define a body) env = defineSymbol a body env
eval (Call a b) env = functionValue a b env
eval _ _ = (Error "Error")