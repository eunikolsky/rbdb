module Encoding
  ( decodeCesu8
  ) where

import Data.ByteString (ByteString)
import Data.Text (Text)

decodeCesu8 :: ByteString -> Text
decodeCesu8 = const ""
