import Parser
import Cpt (cptTest)
import Test.HUnit

main :: IO Counts
main = runTestTT $ TestList [cptTest, parserListTest]
