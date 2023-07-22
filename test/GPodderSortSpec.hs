module GPodderSortSpec (spec) where

import GPodderSort
import Test.Hspec

spec :: Spec
spec = do
  describe "gPodderTitleSortKey" $ do
    it "returns empty string for empty input" $
      gPodderTitleSortKey "" `shouldBe` ""

    it "returns lowercase input" $
      gPodderTitleSortKey "FooBar 8Я ∆" `shouldBe` "foobar 8я ∆"

    it "removes prefix 'the '" $
      gPodderTitleSortKey "The foobar" `shouldBe` "foobar"
