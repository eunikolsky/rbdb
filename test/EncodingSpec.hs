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

    it "decodes single CESU-8 character" $ do
      decodeCesu8 "\xed\xa0\x81\xed\xb0\x80" `shouldBe` "𐐀"
