module EvalSpec (spec) where

import Test.Hspec
import Eval
import Lib

spec :: Spec
spec = do
    describe "empty" $ do
        it "empty" $ do
            1 `shouldBe` 1
