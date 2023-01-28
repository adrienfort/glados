module CPT.Parser where

data Parser a = Parser {
runParser :: String -> Maybe (a , String )
}

parseChar :: Char -> Parser Char
parseChar a = Parser $ \ input -> case input of
    (b:bx) | a == b -> Just (a, bx)
    (_:_) -> Nothing
    [] -> Nothing

parseAnyChar :: String -> Parser Char
parseAnyChar [] = Parser $ \ _ -> Nothing
parseAnyChar (a:ax) = Parser $ \ input -> case input of
    (b:bx) | a == b -> Just (a, bx)
    (_:_) -> runParser (parseAnyChar ax) input
    [] -> Nothing

parserOr :: Parser a -> Parser a -> Parser a
parserOr p1 p2 = Parser $ \ s -> case runParser p1 s of
                    Just (a, s') -> Just (a, s')
                    Nothing -> runParser p2 s

parseAnd :: Parser a -> Parser b -> Parser (a, b)
parseAnd p1 p2 = Parser $ \ s -> case runParser p1 s of
                    Just (a, s') -> case runParser p2 s' of
                                    Just (b, s'') -> Just ((a, b), s'')
                                    Nothing -> Nothing
                    Nothing -> Nothing

parseAndWith :: ( a -> b -> c ) -> Parser a -> Parser b -> Parser c
parseAndWith f p1 p2 = Parser $ \ s -> case runParser p1 s of
                    Just (a, s') -> case runParser p2 s' of
                                    Just (b, s'') -> Just (f a b, s'')
                                    Nothing -> Nothing
                    Nothing -> Nothing

parseMany :: Parser a -> Parser [a]
parseMany p = Parser $ \ s -> case runParser p s of
                Just (a, s') -> case runParser (parseMany p) s' of
                                Just (as, s'') -> Just (a:as, s'')
                                Nothing -> Just ([a], s')
                Nothing -> Just ([], s)

parseSome :: Parser a -> Parser [a]
parseSome p = Parser $ \ s -> case runParser p s of
                Just (a, s') -> case runParser (parseMany p) s' of
                                Just (as, s'') -> Just (a:as, s'')
                                Nothing -> Nothing
                Nothing -> Nothing

parseUInt :: Parser Int
parseUInt = Parser $ \ s -> case runParser (parseSome (parseAnyChar [ '0' .. '9'])) s of
                            Just (as, s') -> Just (read as, s')
                            Nothing -> Nothing

parseInt :: Parser Int
parseInt = Parser $ \ s -> case runParser (parserOr (parseChar '-') (parseChar '+')) s of
                            Just (c, s') -> case runParser parseUInt s' of
                                            Just (i, s'') -> Just (if c == '-' then -i else i, s'')
                                            Nothing -> Nothing
                            Nothing -> runParser parseUInt s
