module Cpt where
import CPT.Cpt
import Test.HUnit

--parseSourceCode "1"
--[Integer 1]

parseSourceCodeSucceeds :: Test
parseSourceCodeSucceeds = TestCase $ assertEqual "parseSourceCodeSucceeds" (CptSymbols "foo") (parseSourceCode "foo")

parseSourceCodeSuccess2 :: Test
parseSourceCodeSuccess2 = TestCase $ assertEqual "parseSourceCodeSuccess2" (CptInteger 42) (parseSourceCode "42")

parseSourceCodeSuccess3 :: Test
parseSourceCodeSuccess3 = TestCase $ assertEqual "parseSourceCodeSuccess3" (CptLists [CptSymbols "foo",CptInteger 42]) (parseSourceCode "(foo 42)")

parseSourceCodeSuccess4 :: Test
parseSourceCodeSuccess4 = TestCase $ assertEqual "parseSourceCodeSuccess4" (CptLists [CptLists [CptSymbols "define",CptSymbols "add",CptLists [CptSymbols "lambda",CptLists [CptSymbols "a",CptSymbols "b"],CptLists [CptSymbols "+",CptSymbols "a",CptSymbols "b"]]],CptLists [CptSymbols "add",CptInteger 3,CptInteger 4]]) (parseSourceCode "(define add\n  (lambda (a b)\n    (+ a b)))\n(add 3 4)")

parseSourceCodeSuccess5 :: Test
parseSourceCodeSuccess5 = TestCase $ assertEqual "parseSourceCodeSuccess5" (CptLists [CptLists [CptLists [CptLists [CptInteger 1,CptLists [CptInteger 2],CptInteger 3]]]]) (parseSourceCode "((((1(2) 3) ) ) )")

parseSourceCodeSuccess6 :: Test
parseSourceCodeSuccess6 = TestCase $ assertEqual "parseSourceCodeSuccess6" (CptLists [CptInteger 1,CptInteger 2,CptInteger 3]) (parseSourceCode "(1 2 3)")

parseSourceCodeSuccess7 :: Test
parseSourceCodeSuccess7 = TestCase $ assertEqual "ParseSourceCodeSuccess7" (CptLists [CptLists [CptSymbols "foo",CptSymbols "bar",CptSymbols "baz"],CptLists [CptInteger 1,CptInteger 2,CptInteger 3],CptLists [],CptLists [CptLists [CptLists [CptLists [CptInteger 1,CptLists [CptInteger 2],CptInteger 3]]]]]) (parseSourceCode "(( foo bar baz )\n(1 2 3) ()\n((((1(2) 3) ) ) )\n)")

cptTest :: Test
cptTest =
  TestList
    [
      parseSourceCodeSucceeds,
      parseSourceCodeSuccess2,
      parseSourceCodeSuccess3,
      parseSourceCodeSuccess4,
      parseSourceCodeSuccess5,
      parseSourceCodeSuccess6,
      parseSourceCodeSuccess7
    ]