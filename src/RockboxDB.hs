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

newtype EntriesCount = EntriesCount Word32
  deriving stock Show

-- | Parsed rockbox database, contains the number of entries.
newtype Database = Database EntriesCount
  deriving stock Show

word32 :: Parser Word32
word32 = word32le

dbParser :: Parser Database
dbParser = do
  _magic <- string "\x0f\x48\x43\x54"
  _dataSize <- word32
  numEntries <- word32
  _serial <- word32
  _commitId <- word32
  _isDirty <- word32

  let entrySizeWords = 22
  void $ count (fromIntegral numEntries * entrySizeWords) word32

  {-
   - not expecting an EOF because of the mismatch of the declared data size,
   - number of entries and the filesize:
   - # of entries in bytes < filesize < declared data size
   -}
  -- eof

  pure $ Database $ EntriesCount numEntries
