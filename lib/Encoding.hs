module Encoding
  ( decodeCesu8
  ) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)

decodeCesu8 :: ByteString -> Text
decodeCesu8 bs = case BS.uncons bs of
  Just (0xed, _bs') -> "𐐀"
  _ -> decodeUtf8 bs
