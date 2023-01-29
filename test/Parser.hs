module Parser where
import CPT.Parser
import Test.HUnit

parseCharFails :: Test
parseCharFails = TestCase $ assertEqual "parseCharFails" Nothing (runParser (parseChar 'a') "b")

parseCharSucceeds :: Test
parseCharSucceeds = TestCase $ assertEqual "parseCharSucceeds" (Just ('a', "b")) (runParser (parseChar 'a') "ab")

parserAnyCharFails :: Test
parserAnyCharFails = TestCase $ assertEqual "parserAnyCharFails" Nothing (runParser (parseAnyChar "abc") "d")

parserAnyCharSucceeds :: Test
parserAnyCharSucceeds = TestCase $ assertEqual "parserAnyCharSucceeds" (Just ('a', "bc")) (runParser (parseAnyChar "abc") "abc")

parseOrSucceeds :: Test
parseOrSucceeds = TestCase $ assertEqual "parseOrTest" (Just ('a', "bcd")) (runParser (parseOr (parseChar 'a') (parseChar 'b')) "abcd")

parseOrFails :: Test
parseOrFails = TestCase $ assertEqual "parseOrFails" Nothing (runParser (parseOr (parseChar 'a') (parseChar 'b')) "cde")

parseOrSucceeds2 :: Test
parseOrSucceeds2 = TestCase $ assertEqual "parseOrSucceeds2" (Just ('b', "cda")) (runParser (parseOr (parseChar 'a') (parseChar 'b')) "bcda")

parseAndSucceeds :: Test
parseAndSucceeds = TestCase $ assertEqual "parseAndSucceeds" (Just (('a', 'b'), "cd")) (runParser (parseAnd (parseChar 'a') (parseChar 'b')) "abcd")

parseAndSuccess2 :: Test
parseAndSuccess2 = TestCase $ assertEqual "parseAndSuccess2" Nothing (runParser (parseAnd (parseChar 'a') (parseChar 'b')) "bcda")

-- parseAnd ( parseChar 'a ') ( parseChar 'b ') " acd "
parseAndSuccess3 :: Test
parseAndSuccess3 = TestCase $ assertEqual "parseAndSuccess3" Nothing (runParser (parseAnd (parseChar 'a') (parseChar 'b')) "acd")

parseAndFails :: Test
parseAndFails = TestCase $ assertEqual "parseAndFails" Nothing (runParser (parseAnd (parseChar 'a') (parseChar 'b')) "cde")

-- parseAndWith (\ x y -> [x , y ]) ( parseChar 'a ') ( parseChar 'b ') " abcd "
parseAndWithSucceeds :: Test
parseAndWithSucceeds = TestCase $ assertEqual "parseAndWithSucceeds" (Just ("ab", "cd")) (runParser (parseAndWith (\x y -> [x, y]) (parseChar 'a') (parseChar 'b')) "abcd")

parseAndWithSucceeds2 :: Test
parseAndWithSucceeds2 = TestCase $ assertEqual "parseAndWithSucceeds2" Nothing (runParser (parseAndWith (\x y -> [x, y]) (parseChar 'a') (parseChar 'b')) "bcda")

parseAndWithSucceeds3 :: Test
parseAndWithSucceeds3 = TestCase $ assertEqual "parseAndWithSucceeds3" Nothing (runParser (parseAndWith (\x y -> [x, y]) (parseChar 'a') (parseChar 'b')) "acd")

parseAndWithFail :: Test
parseAndWithFail = TestCase $ assertEqual "parseAndWithFail" Nothing (runParser (parseAndWith (\x y -> [x, y]) (parseChar 'a') (parseChar 'b')) "cde")

parseManySucceeds :: Test
parseManySucceeds = TestCase $ assertEqual "parseManySucceeds" (Just ("     ", "foobar")) (runParser (parseMany (parseChar ' ')) "     foobar")

parseManySucceeds2 :: Test
parseManySucceeds2 = TestCase $ assertEqual "parseManySucceeds2" (Just ("", "foobar    ")) (runParser (parseMany (parseChar ' ')) "foobar    ")

parseManySucceeds3 :: Test
parseManySucceeds3 = TestCase $ assertEqual "parseManySucceeds3" (Just ("", "bcd")) (runParser (parseMany (parseChar 'a')) "bcd")

parseManyFails :: Test
parseManyFails = TestCase $ assertEqual "parseManyFails" (Just ("", "bcd")) (runParser (parseMany (parseChar 'a')) "bcd")

parseSomeSucceeds :: Test
parseSomeSucceeds = TestCase $ assertEqual "parseSomeSucceeds" (Just ("42", "foobar")) (runParser (parseSome (parseAnyChar ['0'..'9'])) "42foobar")

parseSomeSucceeds2 :: Test
parseSomeSucceeds2 = TestCase $ assertEqual "parseSomeSucceeds" Nothing (runParser (parseSome (parseAnyChar ['0'..'9'])) "foobar42")

parseSomeSucceeds3 :: Test
parseSomeSucceeds3 = TestCase $ assertEqual "parseSomeSucceeds" Nothing (runParser (parseSome (parseAnyChar ['0'..'9'])) "foobar")

parseUIntSucceeds :: Test
parseUIntSucceeds = TestCase $ assertEqual "parseUIntSucceeds" (Just (42, "foobar")) (runParser parseUInt "42foobar")

parseUIntSucceeds2 :: Test
parseUIntSucceeds2 = TestCase $ assertEqual "parseUIntSucceeeds2" Nothing (runParser parseUInt "foobar42")

parseUIntSucceeds3 :: Test
parseUIntSucceeds3 = TestCase $ assertEqual "parseUIntSucceeeds3" Nothing (runParser parseUInt "foobar")

parseUIntSucceeds4 :: Test
parseUIntSucceeds4 = TestCase $ assertEqual "parseUIntSucceeeds4" Nothing (runParser parseUInt "-2")

parseIntSucceeds :: Test
parseIntSucceeds = TestCase $ assertEqual "parseIntSucceeds" (Just (42, "foobar")) (runParser parseInt "42foobar")

parseIntSucceeds2 :: Test
parseIntSucceeds2 = TestCase $ assertEqual "parseIntSucceeeds2" (Just (-42, "foobar")) (runParser parseInt "-42foobar")

parseIntSucceeds3 :: Test
parseIntSucceeds3 = TestCase $ assertEqual "parseIntSucceeeds3" Nothing (runParser parseInt "foobar42")

parseIntSucceeds4 :: Test
parseIntSucceeds4 = TestCase $ assertEqual "parseIntSucceeeds4" Nothing (runParser parseInt "foobar")

parseIntSucceeds5 :: Test
parseIntSucceeds5 = TestCase $ assertEqual "parseIntSucceeeds5" (Just (42, "")) (runParser parseInt "+42")

parseIntSucceeds6 :: Test
parseIntSucceeds6 = TestCase $ assertEqual "parseIntSucceeeds6" (Just (42, "foobar")) (runParser parseInt "+42foobar")

parseIntSucceeds7 :: Test
parseIntSucceeds7 = TestCase $ assertEqual "parseIntSucceeeds7" (Just (-42, "foobar")) (runParser parseInt "-42foobar")

-- parsePair parseInt "(123 456)foo bar"
parsePairSucceeds :: Test
parsePairSucceeds = TestCase $ assertEqual "parsePairSucceeds" (Just ((123, 456), "foo bar")) (runParser (parsePair parseInt) "(123 456)foo bar")

parsePairSucceeds2 :: Test
parsePairSucceeds2 = TestCase $ assertEqual "parsePairSucceeds2" Nothing (runParser (parsePair parseInt) "(123 456foo bar")

parsePairSucceeds3 :: Test
parsePairSucceeds3 = TestCase $ assertEqual "parsePairSucceeds3" (Just ((123,456),"foo bar")) (runParser (parsePair parseInt) "(123 456)foo bar")

parseListSucceeds :: Test
parseListSucceeds = TestCase $ assertEqual "parseList" (Just ([1,2,3,4,5],"foo bar")) (runParser (parseList parseInt) "(1 2 3 4 5)foo bar")

parseListSucceeds2 :: Test
parseListSucceeds2 = TestCase $ assertEqual "parseList" Nothing  (runParser (parseList parseInt) "foo bar")

parseListSucceeds3 :: Test
parseListSucceeds3 = TestCase $ assertEqual "parseList" (Just ([1,2,3,4,5],"")) (runParser (parseList parseInt) "(1 2 3 4 5)")

parserListTest :: Test
parserListTest =
  TestList
    [ parseCharFails,
      parseCharSucceeds,
      parserAnyCharFails,
      parserAnyCharSucceeds,
      parseOrSucceeds,
      parseOrFails,
      parseOrSucceeds2,
      parseAndSucceeds,
      parseAndSuccess2,
      parseAndSuccess3,
      parseAndFails,
      parseAndWithSucceeds,
      parseAndWithSucceeds2,
      parseAndWithSucceeds3,
      parseAndWithFail,
      parseManySucceeds,
      parseManySucceeds2,
      parseManySucceeds3,
      parseManyFails,
      parseSomeSucceeds,
      parseSomeSucceeds2,
      parseSomeSucceeds3,
      parseUIntSucceeds,
      parseUIntSucceeds2,
      parseUIntSucceeds3,
      parseUIntSucceeds4,
      parseIntSucceeds,
      parseIntSucceeds2,
      parseIntSucceeds3,
      parseIntSucceeds4,
      parseIntSucceeds5,
      parseIntSucceeds6,
      parseIntSucceeds7,
      parsePairSucceeds,
      parsePairSucceeds2,
      parsePairSucceeds3,
      parseListSucceeds,
      parseListSucceeds2,
      parseListSucceeds3
    ]