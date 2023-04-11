module RockboxDB.IndexEntry
  ( IndexEntry(..)
  , IndexEntryFlags
  , entryFlagIsDeleted
  , entryParser
  ) where

import Control.Monad
import Data.Bits
import RockboxDB.Prelude

-- https://www.rockbox.org/wiki/TagcacheDBFormat#Flags
newtype IndexEntryFlags = IndexEntryFlags Word32
  deriving newtype (Eq, Ord, Num, Show)

entryFlagIsDeleted :: IndexEntryFlags -> Bool
entryFlagIsDeleted (IndexEntryFlags w) = (w .&. 0x1) == 0x1

-- | Parsed entry from rockbox database's index file. It's not a type of the
-- final database entry visible to the user because it will include raw data
-- such as offset into other tag files.
newtype IndexEntry = IndexEntry { flags :: IndexEntryFlags }

entryParser :: Parser IndexEntry
entryParser = do
  _items <- count 22 word32
  flags <- IndexEntryFlags <$> word32
  when (flags > 31) $ fail $ "Unexpected flags value (should be < 32): " <> show flags
  pure $ IndexEntry flags
