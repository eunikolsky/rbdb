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
  _dataSize <- word32
  numEntries <- fromIntegral <$> word32
  _serial <- word32
  _commitId <- word32
  _isDirty <- word32

  let entrySizeWords = 22
  void $ count (numEntries * entrySizeWords) word32

  eof
  pure $ Database ()
