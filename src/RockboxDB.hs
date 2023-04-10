module RockboxDB
  ( Database
  , dbParser
  ) where

import Data.List (genericLength)
import RockboxDB.Entry
import RockboxDB.Prelude
import Text.Megaparsec.Byte

-- | Parsed rockbox database.
data Database = Database
  { entriesCount :: Word32
  -- ^ the total number of entries; `Word32` because the number can't be negative
  , validEntriesCount :: Word32
  -- ^ the number of not deleted entries
  }
  deriving stock Show

dbParser :: Parser Database
dbParser = do
  _magic <- string "\x0f\x48\x43\x54"
  -- https://www.rockbox.org/wiki/TagcacheDBFormat#Index_file_format says it's
  -- the number of bytes after the header, but it's more complicated than that:
  -- https://www.rockbox.org/mail/archive/rockbox-dev-archive-2008-10/0070.shtml
  _dataSize <- word32
  numEntries <- word32
  _serial <- word32
  _commitId <- word32
  _isDirty <- word32

  entries <- count (fromIntegral numEntries) entryParser

  -- TODO also verify data size
  eof

  pure $ Database
    { entriesCount = genericLength entries
    , validEntriesCount = genericLength $ filter (not . entryFlagIsDeleted . getFlags) entries
    }
