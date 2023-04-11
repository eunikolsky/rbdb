module RockboxDB
  ( Database
  , DatabaseDir(..)
  , parse
  ) where

import Data.ByteString qualified as BS
import Data.IntMap ((!?))
import Data.Maybe
import Data.Text qualified as T
import RockboxDB.IndexEntry (IndexEntry(..))
import RockboxDB.IndexEntry qualified as IndexEntry
import RockboxDB.Prelude
import RockboxDB.TagFile.Filename qualified as TagFile (Filenames(..))
import RockboxDB.TagFile.Filename qualified as Filename (getFilename, parser)
import System.FilePath

-- | Parsed rockbox database, consists of only non-deleted entries.
newtype Database = Database { validEntries :: [Entry] }
  deriving stock Show

-- | Parsed valid rockbox database entry.
newtype Entry = Entry FilePath
  deriving newtype Show

-- | Directory of rockbox database, which should contain at least
-- `database_idx.tcd` and `database_4.tcd`.
newtype DatabaseDir = DatabaseDir FilePath

-- | Parses the rockbox database from the given directory.
parse :: DatabaseDir -> IO (ParseErrorOr Database)
parse (DatabaseDir dir) = do
  let indexFile = dir </> "database_idx.tcd"
      filenameTagFile = dir </> "database_4.tcd"

  indexBytes <- BS.readFile indexFile
  filenameBytes <- BS.readFile filenameTagFile

  pure $ do -- in `ParseErrorOr`
    filenames <- runParser Filename.parser filenameTagFile filenameBytes
    runParser (parser filenames) indexFile indexBytes

-- The parser isn't exported because the database is split into multiple files.
parser :: TagFile.Filenames -> Parser Database
parser (TagFile.Filenames filenameMap) = do
  _magic <- string "\x0f\x48\x43\x54"
  -- https://www.rockbox.org/wiki/TagcacheDBFormat#Index_file_format says it's
  -- the number of bytes after the header, but it's more complicated than that:
  -- https://www.rockbox.org/mail/archive/rockbox-dev-archive-2008-10/0070.shtml
  _dataSize <- word32
  numEntries <- word32
  _serial <- word32
  _commitId <- word32
  _isDirty <- word32

  validEntries <- fmap catMaybes . count (fromIntegral numEntries) $ do
    IndexEntry { maybeFilenameOffset } <- IndexEntry.parser
    pure $ do
      filenameOffset <- maybeFilenameOffset
      case filenameMap !? fromIntegral filenameOffset of
        Just filename -> Just . Entry . T.unpack . Filename.getFilename $ filename
        Nothing -> fail $ "Can't find filename at offset " <> show filenameOffset

  -- TODO also verify data size
  eof

  pure $ Database { validEntries }
