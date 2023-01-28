module Parser where
import CPT.Parser
import Test.HUnit

parseCharFails :: Test
parseCharFails = TestCase $ assertEqual "parseCharFails" Nothing (runParser (parseChar 'a') "b")

parserListTest :: Test
parserListTest =
  TestList
    [ parseCharFails ]