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
decodeCesu8 bs = case BS.uncons bs of
  Just (0xed, bs') -> decodeCesu8Char $ BS.take 5 bs'
  _ -> decodeUtf8 bs

decodeCesu8Char :: HasCallStack => ByteString -> Text
decodeCesu8Char bs =
  -- this idea is from https://en.wikipedia.org/wiki/CESU-8
  let part0 = fromIntegral $ (bs `BS.index` 0) .&. 0b00001111
      part1 = fromIntegral $ (bs `BS.index` 1) .&. 0b00111111
      -- #2 is always 0xed
      part2 = fromIntegral $ (bs `BS.index` 3) .&. 0b00001111
      part3 = fromIntegral $ (bs `BS.index` 4) .&. 0b00111111

      code = ((part0 + 1) `shiftL` (6 + 4 + 6))
        .|. (part1 `shiftL` (6 + 4))
        .|. (part2 `shiftL` 6)
        .|. part3
  in T.singleton $ chr code
