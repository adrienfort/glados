module Cpt where
import CPT.Cpt
import Test.HUnit

parseSourceCodeSucceeds :: Test
parseSourceCodeSucceeds = TestCase $ assertEqual "parseSourceCodeSucceeds" (Just (Symbols "foo","")) (parseSourceCode "foo")

parseSourceCodeSuccess2 :: Test
parseSourceCodeSuccess2 = TestCase $ assertEqual "parseSourceCodeSuccess2" (Just (Integer 42, "")) (parseSourceCode "42")

parseSourceCodeSuccess3 :: Test
parseSourceCodeSuccess3 = TestCase $ assertEqual "parseSourceCodeSuccess3" (Just (Lists [Symbols "foo", Integer 42], "")) (parseSourceCode "(foo 42)")

cptTest :: Test
cptTest =
  TestList
    [
      parseSourceCodeSucceeds,
      parseSourceCodeSuccess2,
      parseSourceCodeSuccess3
    ]