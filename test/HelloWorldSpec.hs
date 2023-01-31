module HelloWorldSpec (helloWorldSpec) where

import Test.Hspec
import HelloWorld

helloWorldSpec :: IO ()
helloWorldSpec = hspec $ do
  describe "Hello World" $ do
    it "should return Hello World" $ do
      helloWorld `shouldBe` "Hello World"