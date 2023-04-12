module RockboxDB.IndexEntry
  ( IndexEntry(..)
  , parser
  ) where

import Control.Monad
import RockboxDB.IndexEntry.Flags
import RockboxDB.IndexEntry.Flags qualified as Flags
import RockboxDB.Prelude

-- | Parsed entry from rockbox database's index file. It's not a type of the
-- final database entry visible to the user because it includes raw data
-- such as offset into other tag files.
data IndexEntry = IndexEntry
  { flags :: Flags
  , maybeFilenameOffset :: Maybe Word32
  -- ^ the offset is present only for non-deleted entries
  , playCount :: Word32
  , playTime :: Word32
  , lastPlayed :: Word32
  , mtime :: Word32
  , lastOffset :: Word32
  , lastElapsed :: Word32
  }

parser :: Parser IndexEntry
parser = do
  skip 4
  filenameOffset <- word32
  skip 9
  playCount <- word32
  skip 1
  playTime <- word32
  lastPlayed <- word32
  skip 1
  mtime <- word32
  lastOffset <- word32
  lastElapsed <- word32

  flagsWord <- word32
  flags <- either fail pure $ mkFlags flagsWord

  pure $ IndexEntry
    { flags
    , maybeFilenameOffset = if Flags.isDeleted flags then Nothing else Just filenameOffset
    , playCount
    , playTime
    , lastPlayed
    , mtime
    , lastOffset
    , lastElapsed
    }

skip :: Int -> Parser ()
skip n = void $ count n word32
