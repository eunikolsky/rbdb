module RockboxDB.Entry
  ( Entry(..)
  , EntryFlags
  , entryFlagIsDeleted
  , entryParser
  ) where

import Control.Monad
import Data.Bits
import RockboxDB.Prelude

-- https://www.rockbox.org/wiki/TagcacheDBFormat#Flags
newtype EntryFlags = EntryFlags Word32
  deriving newtype (Eq, Ord, Num, Show)

entryFlagIsDeleted :: EntryFlags -> Bool
entryFlagIsDeleted (EntryFlags w) = (w .&. 0x1) == 0x1

-- | Parsed entry from rockbox database's index file.
newtype Entry = Entry { getFlags :: EntryFlags }

entryParser :: Parser Entry
entryParser = do
  _items <- count 22 word32
  flags <- EntryFlags <$> word32
  when (flags > 31) $ fail $ "Unexpected flags value (should be < 32): " <> show flags
  pure $ Entry flags
