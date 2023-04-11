module RockboxDB.IndexEntry
  ( IndexEntry(..)
  , parser
  ) where

import RockboxDB.IndexEntry.Flags
import RockboxDB.Prelude

-- | Parsed entry from rockbox database's index file. It's not a type of the
-- final database entry visible to the user because it will include raw data
-- such as offset into other tag files.
newtype IndexEntry = IndexEntry { flags :: Flags }

parser :: Parser IndexEntry
parser = do
  _items <- count 22 word32
  flagsWord <- word32
  flags <- either fail pure $ mkFlags flagsWord
  pure $ IndexEntry flags
