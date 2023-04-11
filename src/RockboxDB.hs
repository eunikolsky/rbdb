module RockboxDB
  ( Database
  , DatabaseDir(..)
  , parse
  ) where

import Data.ByteString qualified as BS
import Data.List (genericLength)
import RockboxDB.IndexEntry
import RockboxDB.Prelude
import System.FilePath
import Text.Megaparsec.Byte

-- | Parsed rockbox database.
data Database = Database
  { entriesCount :: Word32
  -- ^ the total number of entries; `Word32` because the number can't be negative
  , validEntriesCount :: Word32
  -- ^ the number of not deleted entries
  }
  deriving stock Show

-- | Directory of rockbox database, which should contain at least
-- `database_idx.tcd` and `database_4.tcd`.
newtype DatabaseDir = DatabaseDir FilePath

-- | Parses the rockbox database from the given directory.
parse :: DatabaseDir -> IO (ParseErrorOr Database)
parse (DatabaseDir dir) = do
  let indexFile = dir </> "database_idx.tcd"
  bytes <- BS.readFile indexFile
  pure $ runParser dbParser indexFile bytes

-- The parser isn't exported because the database is split into multiple files.
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
    , validEntriesCount = genericLength $ filter (not . entryFlagIsDeleted . flags) entries
    }
