module RockboxDB
  ( Database
  , dbParser
  ) where

import Data.ByteString (ByteString)
import Data.List (genericLength)
import Data.Void
import Data.Word
import RockboxDB.Entry
import Text.Megaparsec
import Text.Megaparsec.Byte
import Text.Megaparsec.Byte.Binary

type Parser = Parsec Void ByteString

-- | Parsed rockbox database.
data Database = Database
  { entriesCount :: Word32
  -- ^ the total number of entries; `Word32` because the number can't be negative
  , validEntriesCount :: Word32
  -- ^ the number of not deleted entries
  }
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

  entries <- count (fromIntegral numEntries) entryParser

  {-
   - not expecting an EOF because of the mismatch of the declared data size,
   - number of entries and the filesize:
   - # of entries in bytes < filesize < declared data size
   -}
  -- eof

  pure $ Database
    { entriesCount = genericLength entries
    , validEntriesCount = genericLength $ filter (not . entryFlagIsDeleted . getFlags) entries
    }
