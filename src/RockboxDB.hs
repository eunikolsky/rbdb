module RockboxDB
  ( Database(..)
  , DatabaseDir(..)
  , parse
  ) where

import Control.Monad
import Data.ByteString qualified as BS
import Data.Maybe
import RockboxDB.Entry (Entry)
import RockboxDB.Entry qualified as Entry (parser)
import RockboxDB.Prelude
import RockboxDB.TagFile.Filename qualified as Filename (parser)
import RockboxDB.TagFile.Filename qualified as TagFile (Filenames(..), headerSize)
import System.FilePath
import System.Directory

-- | Parsed rockbox database, consists of only non-deleted entries.
newtype Database = Database { validEntries :: [Entry] }
  deriving stock Show

-- | Directory of rockbox database, which should contain at least
-- `database_idx.tcd` and `database_4.tcd`.
newtype DatabaseDir = DatabaseDir FilePath

-- | Parses the rockbox database from the given directory.
parse :: DatabaseDir -> IO (ParseErrorOr Database)
parse dbDir@(DatabaseDir dir) = do
  let indexFile = dir </> "database_idx.tcd"
      filenameTagFile = dir </> "database_4.tcd"

  indexBytes <- BS.readFile indexFile
  filenameBytes <- BS.readFile filenameTagFile
  expectedDataSize <- getExpectedDataSize dbDir

  pure $ do -- in `ParseErrorOr`
    filenames <- runParser Filename.parser filenameTagFile filenameBytes
    runParser (parser expectedDataSize filenames) indexFile indexBytes

-- The parser isn't exported because the database is split into multiple files.
parser :: Int -> TagFile.Filenames -> Parser Database
parser expectedDataSize filenames = do
  _magic <- string "\x0f\x48\x43\x54"
  dataSize <- word32
  when (expectedDataSize /= fromIntegral dataSize) $ fail $ mconcat
    ["Unexpected data size ", show dataSize, ", should be ", show expectedDataSize]

  numEntries <- word32
  _serial <- word32
  _commitId <- word32
  _isDirty <- word32

  validEntries <- fmap catMaybes . count (fromIntegral numEntries) $
    Entry.parser filenames

  eof

  pure $ Database { validEntries }

-- | Calculates the expected data size in the index file based on the filesizes
-- of almost all the database files.
--
-- https://www.rockbox.org/wiki/TagcacheDBFormat#Index_file_format says it's
-- the number of bytes after the header, but it's more complicated than that:
-- https://www.rockbox.org/mail/archive/rockbox-dev-archive-2008-10/0070.shtml
getExpectedDataSize :: DatabaseDir -> IO Int
getExpectedDataSize (DatabaseDir dir) = do
  indexFileSize <- getFileSize $ dir </> "database_idx.tcd"
  let tagFiles =
        [ dir </> "database_" <> show @Int i <.> "tcd"
        | i <- [0..3] <> [5..8]
        ]
  tagFilesSizes <- traverse getFileSize tagFiles
  let headerSizes = length tagFiles * TagFile.headerSize
  pure . subtract headerSizes . fromIntegral . sum $ indexFileSize : tagFilesSizes
