module HelloWorldSpec (spec) where

import Test.Hspec
import HelloWorld

spec :: Spec
spec = do
  describe "Hello World" $ do
    it "should return Hello World" $ do
      helloWorld `shouldBe` "Hello World"