module Cpt where
import CPT.Cpt
import Test.HUnit

parseSourceCodeSucceeds :: Test
parseSourceCodeSucceeds = TestCase $ assertEqual "parseSourceCodeSucceeds" (Just (Symbols "foo","")) (parseSourceCode "foo")

parseSourceCodeSuccess2 :: Test
parseSourceCodeSuccess2 = TestCase $ assertEqual "parseSourceCodeSuccess2" (Just (Integer 42, "")) (parseSourceCode "42")

parseSourceCodeSuccess3 :: Test
parseSourceCodeSuccess3 = TestCase $ assertEqual "parseSourceCodeSuccess3" (Just (Lists [Symbols "foo", Integer 42], "")) (parseSourceCode "(foo 42)")

parseSourceCodeSuccess4 :: Test
parseSourceCodeSuccess4 = TestCase $ assertEqual "parseSourceCodeSuccess4" (Just (Lists [Lists [Symbols "foo", Symbols "bar", Symbols "baz"], Lists [Integer 1, Integer 2, Integer 3], Lists []], "")) (parseSourceCode "(( foo bar baz )(1 2 3) ())")

parseSourceCodeSuccess5 :: Test
parseSourceCodeSuccess5 = TestCase $ assertEqual "parseSourceCodeSuccess5" (Just (Lists [Lists [Lists [Lists [Integer 1,Lists [Integer 2],Integer 3]]]],"")) (parseSourceCode "((((1(2) 3) ) ) )")

parseSourceCodeSuccess6 :: Test
parseSourceCodeSuccess6 = TestCase $ assertEqual "parseSourceCodeSuccess6" (Just (Lists [Lists [Symbols "foo", Symbols "bar", Symbols "baz"], Lists [Integer 1, Integer 2, Integer 3], Lists []], "")) (parseSourceCode "(( foo bar baz )(1 2 3) ())")
cptTest :: Test
cptTest =
  TestList
    [
      parseSourceCodeSucceeds,
      parseSourceCodeSuccess2,
      parseSourceCodeSuccess3,
      parseSourceCodeSuccess4,
      parseSourceCodeSuccess5,
      parseSourceCodeSuccess6
    ]