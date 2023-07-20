module Encoding
  ( decodeCesu8
  ) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)

decodeCesu8 :: ByteString -> Text
decodeCesu8 = decodeUtf8
