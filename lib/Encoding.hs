module Encoding
  ( decodeCesu8
  ) where

import Data.Bits
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Char
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import GHC.Stack

decodeCesu8 :: HasCallStack => ByteString -> Text
decodeCesu8 = iter ""
  where
    iter :: Text -> ByteString -> Text
    iter acc "" = acc
    iter acc bs =
      let (decodable, rest) = BS.break (== 0xed) bs
          prefix = decodeUtf8 decodable
          (cesu8Char, rest') = BS.splitAt 6 rest
          char = if BS.null rest
            then ""
            else decodeCesu8Char cesu8Char
      in iter (acc <> prefix <> char) rest'

decodeCesu8Char :: HasCallStack => ByteString -> Text
decodeCesu8Char bs =
  -- this idea is from https://en.wikipedia.org/wiki/CESU-8
  let -- #0 is always 0xed
      part0 = fromIntegral $ (bs `BS.index` 1) .&. 0b00001111
      part1 = fromIntegral $ (bs `BS.index` 2) .&. 0b00111111
      -- #3 is always 0xed
      part2 = fromIntegral $ (bs `BS.index` 4) .&. 0b00001111
      part3 = fromIntegral $ (bs `BS.index` 5) .&. 0b00111111

      code = ((part0 + 1) `shiftL` (6 + 4 + 6))
        .|. (part1 `shiftL` (6 + 4))
        .|. (part2 `shiftL` 6)
        .|. part3
  in T.singleton $ chr code
