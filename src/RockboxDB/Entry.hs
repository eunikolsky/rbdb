module RockboxDB.Entry
  ( Entry(..)
  , EntryFlags
  , entryFlagIsDeleted
  , entryParser
  ) where

import Data.Bits
import Data.ByteString (ByteString)
import Data.Void
import Data.Word
import Text.Megaparsec
import Text.Megaparsec.Byte.Binary

type Parser = Parsec Void ByteString

-- https://www.rockbox.org/wiki/TagcacheDBFormat#Flags
newtype EntryFlags = EntryFlags Word32

entryFlagIsDeleted :: EntryFlags -> Bool
entryFlagIsDeleted (EntryFlags w) = (w .&. 0x1) == 0x1

-- | Parsed entry from rockbox database's index file.
newtype Entry = Entry { getFlags :: EntryFlags }

word32 :: Parser Word32
word32 = word32le

entryParser :: Parser Entry
entryParser = do
  _items <- count 21 word32
  flags <- EntryFlags <$> word32
  pure $ Entry flags
