module EpisodeEntrySpec (spec) where

import Data.List (sortOn)
import EpisodeEntry
import GPodderSort
import Test.Hspec

spec :: Spec
spec = do
  describe "EpisodePath" $ do
    it "can be used to sort by podcast and episode" $
      let input =
            [ EpisodePath "/podcasts/" "eax" "a"
            , EpisodePath "/podcasts/" "abc" "x"
            , EpisodePath "/podcasts/" "eax" "b"
            , EpisodePath "/podcasts/" "abc" "c"
            ]

          expected =
            [ EpisodePath "/podcasts/" "abc" "c"
            , EpisodePath "/podcasts/" "abc" "x"
            , EpisodePath "/podcasts/" "eax" "a"
            , EpisodePath "/podcasts/" "eax" "b"
            ]

      in sortOn (\x -> (gPodderTitleSortKey . podcast $ x, episode x)) input
          `shouldBe` expected
