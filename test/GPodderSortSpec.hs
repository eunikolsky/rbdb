module GPodderSortSpec (spec) where

import Control.Monad
import Data.List (sortOn)
import Data.Text.Lazy qualified as TL
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
      it ("replaces " <> TL.unpack (TL.singleton input) <> " with " <> [expected]) $
        gPodderTitleSortKey (TL.singleton input) `shouldBe` TL.singleton expected

    it "can be used to sort podcast titles" $
      let input = ["zulu", "The box", "подкаст", "The chapter", "the Abstraction"]
          expected = ["the Abstraction", "The box", "The chapter", "zulu", "подкаст"]
      in sortOn gPodderTitleSortKey input `shouldBe` expected
