module EncodingSpec (spec) where

import Encoding
import Test.Hspec

spec :: Spec
spec = do
  describe "decodeCesu8" $ do
    it "returns empty string for empty input" $ do
      decodeCesu8 "" `shouldBe` ""

    it "decodes UTF-8 properly" $ do
      decodeCesu8 "ab C \xd0\xa4\xd1\x94\xe2\x88\x9e \xc5\x8f\xe2\xa9\x94\xe2\x9d\x86\xef\xbf\xbd"
        `shouldBe` "ab C Фє∞ ŏ⩔❆�"
