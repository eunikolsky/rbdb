module RockboxDB
  ( Database
  , dbParser
  ) where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Void
import Data.Word
import Text.Megaparsec
import Text.Megaparsec.Byte
import Text.Megaparsec.Byte.Binary

type Parser = Parsec Void ByteString

-- | Parsed rockbox database.
newtype Database = Database ()
  deriving stock Show

word32 :: Parser Word32
word32 = word32le

dbParser :: Parser Database
dbParser = do
  _magic <- string "\x0f\x48\x43\x54"
  dataSize <- fromIntegral <$> word32
  _numEntries <- word32
  _serial <- word32
  _commitId <- word32
  _isDirty <- word32

  void $ count dataSize word8

  eof
  pure $ Database ()
