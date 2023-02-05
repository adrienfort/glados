module Cpt where
import CPT.Cpt
import Test.HUnit

--parseSourceCode "1"
--[Integer 1]

parseSourceCodeSucceeds :: Test
parseSourceCodeSucceeds = TestCase $ assertEqual "parseSourceCodeSucceeds" ([Symbols "foo"]) (parseSourceCode "foo")

parseSourceCodeSuccess2 :: Test
parseSourceCodeSuccess2 = TestCase $ assertEqual "parseSourceCodeSuccess2" ([Integer 42]) (parseSourceCode "42")

parseSourceCodeSuccess3 :: Test
parseSourceCodeSuccess3 = TestCase $ assertEqual "parseSourceCodeSuccess3" ([Lists [Symbols "foo", Integer 42]]) (parseSourceCode "(foo 42)")

parseSourceCodeSuccess4 :: Test
parseSourceCodeSuccess4 = TestCase $ assertEqual "parseSourceCodeSuccess4" ([Lists [Lists [Symbols "foo", Symbols "bar", Symbols "baz"], Lists [Integer 1, Integer 2, Integer 3], Lists []]]) (parseSourceCode "(( foo bar baz )(1 2 3) ())")

parseSourceCodeSuccess5 :: Test
parseSourceCodeSuccess5 = TestCase $ assertEqual "parseSourceCodeSuccess5" ([Lists [Lists [Lists [Lists [Integer 1,Lists [Integer 2],Integer 3]]]]]) (parseSourceCode "((((1(2) 3) ) ) )")

parseSourceCodeSuccess6 :: Test
parseSourceCodeSuccess6 = TestCase $ assertEqual "parseSourceCodeSuccess6" ([Lists [Integer 1,Integer 2,Integer 3]]) (parseSourceCode "(1 2 3)")

parseSourceCodeSuccess7 :: Test
parseSourceCodeSuccess7 = TestCase $ assertEqual "ParseSourceCodeSuccess7" ([Lists [Symbols "define",Symbols "add",Lists [Symbols "lambda",Lists [Symbols "a",Symbols "b"],Lists [Symbols "+",Symbols "a",Symbols "b"]]],Lists [Symbols "add",Integer 3,Integer 4]]) (parseSourceCode "(define add (lambda (a b) (+ a b))) (add 3 4)")

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