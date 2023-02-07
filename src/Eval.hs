module Eval
    (
        Env,
        Result (..),
        eval,
        evaluate,
        printEvaluation
    ) where

import Lib

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


resultToReturnValue :: Result -> ReturnValue
resultToReturnValue r = case r of
    (Value v) -> (Val v)
    (Bolean v) -> (Bool v)
    (Error err) -> (Err err)
    _ -> (Err ("e"))

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
getBuiltins = [
            ("if", ifcondition),
            ("+", add),
            ("-", minus),
            ("*", mult),
            ("div", division),
            ("mod", modulo),
            ("<", inferiorto),
            ("eq?", equal)
            ]

ifcondition :: Function
ifcondition (a:b:c:[]) env = case eval a env of
    (Bolean "#t") -> case eval b env of -- evaluate first expression
        (Value r) -> (Val r)
        (Bolean r) -> (Bool r)
        (Error err) -> (Err err)
        (_) -> (Err ("Invalid arguments to function if " ++ show a ++ " " ++ show b ++ " " ++ show c))
    (Bolean "#f") -> case eval c env of -- evaluate second expression
        (Value r) -> (Val r)
        (Bolean r) -> (Bool r)
        (Error err) -> (Err err)
        (_) -> (Err ("Invalid arguments to function if " ++ show a ++ " " ++ show b ++ " " ++ show c))
    (Error err) -> (Err err)
    (_) -> (Err ("Invalid arguments to function if " ++ show a ++ " " ++ show b ++ " " ++ show c))
ifcondition _ _ = (Err "Invalid arguments to function if")

equal :: Function
equal (a:b:[]) env = case eval a env of
    (Value a1) -> case eval b env of
        (Value a2) -> case a1 == a2 of 
            True -> (Bool "#t")
            False -> (Bool "#f")
        (Error err) -> (Err err)
        (_) -> (Err ("Invalid arguments to function eq? " ++ show a ++ " " ++ show b))
    (Error err) -> (Err err)
    (_) -> (Err ("Invalid arguments to function eq? " ++ show a ++ " " ++ show b))
equal _ _ = (Err "Invalid arguments to function eq?")

add :: Function
add (a:b:[]) env = case eval a env of
    (Value a1) -> case eval b env of
        (Value a2) -> (Val (a1 + a2))
        (Error err) -> (Err err)
        (_) -> (Err ("Invalid arguments to function + " ++ show a ++ " " ++ show b))
    (Error err) -> (Err err)
    (_) -> (Err ("Invalid arguments to function + " ++ show a ++ " " ++ show b))
add _ _ = (Err "Invalid arguments to function +")

minus :: Function
minus (a:b:[]) env = case eval a env of
    (Value a1) -> case eval b env of
        (Value a2) -> (Val (a1 - a2))
        (Error err) -> (Err err)
        (_) -> (Err ("Invalid arguments to function - " ++ show a ++ " " ++ show b))
    (Error err) -> (Err err)
    (_) -> (Err ("Invalid arguments to function - " ++ show a ++ " " ++ show b))
minus _ _ = (Err "Invalid arguments to function -")

mult :: Function
mult (a:b:[]) env = case eval a env of
    (Value a1) -> case eval b env of
        (Value a2) -> (Val (a1 * a2))
        (Error err) -> (Err err)
        (_) -> (Err ("Invalid arguments to function * " ++ show a ++ " " ++ show b))
    (Error err) -> (Err err)
    (_) -> (Err ("Invalid arguments to function * " ++ show a ++ " " ++ show b))
mult _ _ = (Err "Invalid arguments to function *")

division :: Function
division (a:b:[]) env = case eval a env of
    (Value a1) -> case eval b env of
        (Value 0) -> (Err ("Divide by zero in function div " ++ show a ++ " " ++ show b))
        (Value a2) -> (Val (a1`div`a2))
        (Error err) -> (Err err)
        (_) -> (Err ("Invalid arguments to function div " ++ show a ++ " " ++ show b))
    (Error err) -> (Err err)
    (_) -> (Err ("Invalid arguments to function div " ++ show a ++ " " ++ show b))
division _ _ = (Err "Invalid arguments to function div")

modulo :: Function
modulo (a:b:[]) env = case eval a env of
    (Value a1) -> case eval b env of
        (Value 0) -> (Err ("Divide by zero in function modulo " ++ show a ++ " " ++ show b))
        (Value a2) -> (Val (a1`mod`a2))
        (Error err) -> (Err err)
        (_) -> (Err ("Invalid arguments to function mod " ++ show a ++ " " ++ show b))
    (Error err) -> (Err err)
    (_) -> (Err ("Invalid arguments to function mod " ++ show a ++ " " ++ show b))
modulo _ _ = (Err "Invalid arguments to function mod")

inferiorto :: Function
inferiorto (a:b:[]) env = case eval a env of
    (Value a1) -> case eval b env of
        (Value a2) -> case a1 < a2 of
            True -> (Bool "#t")
            False -> (Bool "#f")
        (Error err) -> (Err err)
        (_) -> (Err ("Invalid arguments to function < " ++ show a ++ " " ++ show b))
    (Error err) -> (Err err)
    (_) -> (Err ("Invalid arguments to function < " ++ show a ++ " " ++ show b))
inferiorto _ _ = (Err "Invalid arguments to function <")


isBuiltin :: [Ast] -> Env -> ReturnValue
isBuiltin (AstSymbol a:b) env = case lookup a getBuiltins of
                    Nothing -> Bool "no"
                    Just bu -> bu b env
isBuiltin _ _ = Err "Bad call"

-- and a function arguments into the env
setFunctionEnv :: [String] -> [Ast] -> Env -> Either Env String
setFunctionEnv [] (_:_) _ = Right "Invalid arguments"
setFunctionEnv (_:_) [] _ = Right "Invalid arguments"
setFunctionEnv [] [] env = Left env
setFunctionEnv (s:sr) (b:br) env = case (eval b env) of
    (Value v) -> setFunctionEnv sr br (addKeyValue s (AstInteger v) env)
    (Bolean v) -> setFunctionEnv sr br (addKeyValue s (AstBoolean v) env)
    (Error err) -> Right err
    _ -> Right ("Func " ++ s ++ ": no expression in body")

callFunc :: [Ast] -> Env -> ReturnValue
-- lambda call
callFunc (AstLambda a e:[]) env = case (length a) == 0 of
        True -> case resultToReturnValue (eval e env) of -- function doesn't need args
            (Err "e") -> (Err ("lambda : incorrect return type"))
            v -> v
        False -> (Err ("Calling lambda with incorrect number of arguments")) -- called func with args
callFunc (AstLambda a e:b) env = case (length a) == (length b) of -- check args nbr
        False -> (Err ("Calling lambda with incorrect number of arguments"))
        True -> case setFunctionEnv a b env of -- insert evaluated args in env
            Left nenv ->case resultToReturnValue (eval e nenv) of
                (Err "e") -> (Err ("lambda : incorrect return type"))
                v -> v
            Right err -> (Err err)
-- function call
callFunc (AstSymbol a:[]) env = case getKeyValue a env of -- call without args
    Right err -> (Err err)
    -- symbol redirect to a function definition
    Left (AstDefine (Right s) e) -> funcWithoutArgs s e
    -- symbol redirect to a lambda function
    Left (AstLambda s e) -> funcWithoutArgs s e
    Left _ -> (Err ("Invalid call " ++ a))
    where
        funcWithoutArgs :: [String] -> Ast -> ReturnValue 
        funcWithoutArgs s e = case (length s) == 0 of 
            True -> case resultToReturnValue (eval e env) of -- function doesn't need args
                (Err "e") -> (Err (show a ++ " : incorrect return type"))
                v -> v
            False -> (Err ("Calling " ++ show a ++ " with incorrect number of arguments")) -- func need args
callFunc (AstSymbol a:b) env = case getKeyValue a env of
    Right err -> (Err err)
    -- symbol redirect to a function definition
    Left (AstDefine (Right s) e) -> funcWithArgs s e
    -- symbol redirect to a lambda function
    Left (AstLambda s e) -> funcWithArgs s e
    Left _ -> (Err ("Invalid call " ++ a)) -- get args
    where
        funcWithArgs :: [String] -> Ast -> ReturnValue
        funcWithArgs s e = case (length s) == (length b) of -- check args nbr
            False -> (Err ("Calling " ++ show a ++ " with incorrect number of arguments"))
            True -> case setFunctionEnv s b env of -- insert evaluated args in env
                Left nenv -> case resultToReturnValue (eval e nenv) of
                    (Err "e") -> (Err (show a ++ " : incorrect return type"))
                    v -> v
                Right err -> (Err err)
callFunc _ _ = (Err "Invalid syntax")


functionValue :: [Ast] -> Env -> Result
functionValue [] _ = (Error "Invalid function call")
functionValue s env = case isBuiltin s env of
    (Val a) -> (Value a) -- check builtin
    (Bool "no") -> case callFunc s env of
        (Val a) -> (Value a)
        (Bool a) -> (Bolean a)
        (Err err) -> (Error err)
    (Bool a) -> (Bolean a)
    (Err err) -> (Error err)
functionValue _ _ = (Error "Invalid function call")

getSymbol :: String -> Env -> Result
getSymbol str env = case getKeyValue str env of
    Right err -> (Error err) -- error : didn't find
    Left (AstInteger a) -> (Value a)
    Left (AstBoolean a) -> (Bolean a)
    Left (AstSymbol a) -> eval (AstSymbol a) env
    Left (AstDefine _ _) -> (Expression ("function " ++ str))
    Left (AstLambda _ _) -> (Expression ("function " ++ str))
    Left _ -> (Error "Unknown value")

defineSymbol :: Either String [String] -> Ast -> Env -> Result
defineSymbol (Left a) body env = case body of -- syntax (define x body)
        (AstDefine _ _) -> (Error ("Symbol " ++ a ++ " invalid assignation"))
        _ -> case eval body env of
            (Environment _) -> (Error ("Symbol " ++ a ++ " invalid assignation (null type)"))
            (Error err) -> (Error err)
            (Value val) -> (Environment (insertKeyValue a (AstInteger val) env)) -- get a int value from either an int, a symbol or a function call
            (Bolean val) -> (Environment (insertKeyValue a (AstBoolean val) env)) -- get a boolean value from either a boolean, a symbol or a function call
            (Expression _) -> (Environment (insertKeyValue a body env)) -- got an expression as return value => either a function or a lambda
defineSymbol (Right []) _ _ = (Error "Symbol name is not defined")
defineSymbol (Right (a:b)) body env = (Environment (insertKeyValue a (AstDefine (Right b) body) env))

eval :: Ast -> Env -> Result
eval (AstInteger a) _ = (Value a)
eval (AstBoolean a) _ = (Bolean a)
eval (AstSymbol a) env = getSymbol a env
eval (AstLambda _ _) _ = (Expression "lambda")
eval (AstDefine a body) env = defineSymbol a body env
eval (AstCall a) env = functionValue a env
eval _ _ = (Error "Error")

evaluate :: [Ast] -> Env -> [Result]
evaluate [] _ = []
evaluate (a:b) env = case eval a env of
    (Environment nenv) -> evaluate b nenv
    (Error err) -> [Error err]
    (res) -> res : evaluate b env

printEvaluation :: [Result] -> IO ()
printEvaluation [] = return ()
printEvaluation (a:b) = putStrLn (show a) >> printEvaluation b
