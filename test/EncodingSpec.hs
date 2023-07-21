module EncodingSpec (spec) where

import Control.Monad
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding
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

      -- these can be generated with:
      -- $ print -n '𐐀' | uconv -t cesu-8 | xxd
      -- 00000000: eda0 81ed b080                           ......

    forM_
      [ ("\xed\xa0\x80\xed\xb0\x80", "𐀀") -- U+10000
      , ("\xed\xa0\x81\xed\xb0\x80", "𐐀") -- U+10400; \xD801\xDC00
      , ("\xed\xa0\xbd\xed\xba\xb2", "🚲") -- U+1F6B2
      -- `􏿿` itself can't be parsed by GHC:
      -- `error: [GHC-21231] lexical error in string/character literal at character '\1114111'`
      , ("\xed\xaf\xbf\xed\xbf\xbf", decodeUtf8 "\xf4\x8f\xbf\xbf") -- U+10FFFF
      ]
      $ \(input, expected) ->
        it ("decodes single CESU-8 character " <> TL.unpack expected) $
          decodeCesu8 input `shouldBe` expected

    it "decodes a single CESU-8 character at the end" $ do
      decodeCesu8 "foo \xd0\xaf <\xed\xa0\x81\xed\xb0\x80" `shouldBe` "foo Я <𐐀"

    it "decodes a single CESU-8 character inside string" $ do
      decodeCesu8 "foo \xd0\xaf <\xed\xa0\x81\xed\xb0\x80! \xe2\x88\x86" `shouldBe` "foo Я <𐐀! ∆"

    it "decodes CESU-8 characters inside string" $ do
      decodeCesu8 "foo \xd0\xaf <\xed\xa0\x81\xed\xb0\x80! \xed\xa0\x80\xed\xb0\x80\xe2\x88\x86" `shouldBe` "foo Я <𐐀! 𐀀∆"

    it "decodes multiple CESU-8 characters" $ do
      decodeCesu8 "\xed\xa0\xbd\xed\xba\xb2\xed\xa0\x81\xed\xb0\x80\xed\xa0\x80\xed\xb0\x80" `shouldBe` "🚲𐐀𐀀"
