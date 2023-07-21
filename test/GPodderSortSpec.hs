module GPodderSortSpec (spec) where

import GPodderSort
import Test.Hspec

spec :: Spec
spec = do
  describe "gPodderTitleSortKey" $ do
    it "returns empty string for empty input" $ do
      gPodderTitleSortKey "" `shouldBe` ""
