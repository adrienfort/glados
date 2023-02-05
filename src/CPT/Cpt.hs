import Data.Char (isDigit, isSpace)

data Cpt = Lists [Cpt] | Symbols String | Integer Int deriving (Eq, Show)

parseTree :: String -> Maybe (Cpt, String)
parseTree s = case s of
    "" -> Nothing
    ('(':xs) -> do
        (cpts, rest) <- parseList xs
        return (Lists cpts, rest)
    (x:xs) | isDigit x -> do
        (num, rest) <- parseInteger (x:xs)
        return (Integer num, rest)
    (x:xs) -> do
        let (symbol, rest) = parseSymbol (x:xs)
        if symbol == "" then parseTree rest else return (Symbols symbol, rest)

parseList :: String -> Maybe ([Cpt], String)
parseList s = case s of
    (')':xs) -> Just ([], xs)
    _ -> do
        (cpt, rest) <- parseTree s
        (cpts, rest') <- parseList rest
        return (cpt:cpts, rest')

parseSymbol :: String -> (String, String)
parseSymbol s = let (symbol, rest) = span (\c -> not (isSpace c) && c /= ')') s in (symbol, dropWhile isSpace $ rest)

parseInteger :: String -> Maybe (Int, String)
parseInteger s = case reads s of
    [(n, rest)] -> Just (n, rest)
    _ -> Nothing

parseSourceCode :: String -> Maybe (Cpt, String)
parseSourceCode s = parseTree (filter (\c -> not (isSpace c) && c /= '\n') s)
