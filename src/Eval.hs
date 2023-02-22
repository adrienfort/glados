module Eval
    (
    ) where

import Lib

type Stack = [Ast]

stackPush :: Stack -> Ast -> Stack
stackPush a v = v : a

stackPop :: Stack -> Stack
stackPop [] = []
stackPop (_:b) = b

envGetKey :: String -> Env -> Either Ast String
envGetKey str env = case lookup str env of
        Nothing -> Right (str ++ " is undefined")
        Just val -> Left val

-- add a key value and can replace an existing key value
envInsertKey :: String -> Ast -> Env -> Env
envInsertKey str ast env = case lookup str env of
    Nothing -> (str, ast) : env
    Just _ -> replaceKey env
        where
            replaceKey [] = []
            replaceKey ((s, val) : b) | s == str = ((str, ast) : b)
                                    | otherwise = ((s, val) : (replaceKey b))

-- add a new key value
envAddKey :: String -> Ast -> Env -> Env
envAddKey str ast env = (str, ast) : env
