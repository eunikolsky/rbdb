module EncodingSpec (spec) where

import Encoding
import Test.Hspec

spec :: Spec
spec = do
  describe "decodeCesu8" $ do
    it "returns empty string for empty input" $ do
      decodeCesu8 "" `shouldBe` ""
