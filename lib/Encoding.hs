module Encoding
  ( decodeCesu8
  ) where

import Data.Bits
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Char
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Builder qualified as TLB
import Data.Text.Encoding (decodeUtf8)
import GHC.Stack

{-|
 - Decodes CESU-8 string into (Unicode) `Text`.
 -
 - CESU-8 is almost UTF-8, except that non-BMP characters (≥ U+10000) are first
 - encoded as a UTF-16BE surrogate pair (two UTF-16BE characters), and then
 - each UTF-16BE character is encoded in UTF-8 (three bytes each).
 -
 - See also: <https://en.wikipedia.org/wiki/CESU-8>.
 -}
decodeCesu8 :: HasCallStack => ByteString -> Text
decodeCesu8 = TLB.toLazyText . iter ""
  where
    iter :: TLB.Builder -> ByteString -> TLB.Builder
    iter acc "" = acc
    iter acc bs =
      -- due to the UTF-8 encoding, `0xed = 0b11101101` can only be the first
      -- byte of a three-byte sequence, so it's fine to look for it without
      -- decoding all UTF-8 characters sequentially.
      -- an alternative implementation would be to use the new `decodeUtf8Chunk`
      -- function from `text-2.0.2` to let it UTF-8 decode everything it can and
      -- manually decode the error sequences (which are CESU-8).
      let (decodable, rest) = BS.break (== 0xed) bs
          prefix = TLB.fromText $ decodeUtf8 decodable
          (cesu8Char, rest') = BS.splitAt 6 rest
          char = if BS.null rest
            then ""
            else decodeCesu8Char cesu8Char
      in iter (acc <> prefix <> char) rest'

decodeCesu8Char :: HasCallStack => ByteString -> TLB.Builder
decodeCesu8Char bs =
  -- the idea of the direct decoding into Unicode is from
  -- https://en.wikipedia.org/wiki/CESU-8; an alternative would be to manually
  -- decode into UTF-16BE and then decode that into Unicode.
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
  in TLB.singleton $ chr code
