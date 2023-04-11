module RockboxDB.IndexEntry
  ( IndexEntry(..)
  , entryParser
  ) where

import Control.Monad
import RockboxDB.IndexEntry.Flags
import RockboxDB.Prelude

-- | Parsed entry from rockbox database's index file. It's not a type of the
-- final database entry visible to the user because it will include raw data
-- such as offset into other tag files.
newtype IndexEntry = IndexEntry { flags :: Flags }

entryParser :: Parser IndexEntry
entryParser = do
  _items <- count 22 word32
  flags <- Flags <$> word32
  when (flags > 31) $ fail $ "Unexpected flags value (should be < 32): " <> show flags
  pure $ IndexEntry flags
