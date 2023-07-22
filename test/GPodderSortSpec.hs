module GPodderSortSpec (spec) where

import Control.Monad
import Data.Text qualified as T
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

    forM_ [('ö', 'o'), ('ü', 'u'), ('ä', 'a')] $ \(input, expected) ->
      it ("replaces " <> T.unpack (T.singleton input) <> " with " <> [expected]) $
        gPodderTitleSortKey (T.singleton input) `shouldBe` T.singleton expected
