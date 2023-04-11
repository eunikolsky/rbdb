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
  }

parser :: Parser IndexEntry
parser = do
  void $ count 4 word32
  filenameOffset <- word32
  void $ count 17 word32

  flagsWord <- word32
  flags <- either fail pure $ mkFlags flagsWord

  pure $ IndexEntry
    { flags
    , maybeFilenameOffset = if Flags.isDeleted flags then Nothing else Just filenameOffset
    }
