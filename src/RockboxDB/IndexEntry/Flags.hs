module RockboxDB.IndexEntry.Flags
  ( Flags(..)
  , isDeleted
  ) where

import Data.Bits
import RockboxDB.Prelude

-- https://www.rockbox.org/wiki/TagcacheDBFormat#Flags
newtype Flags = Flags Word32
  deriving newtype (Eq, Ord, Num, Show)

deleted :: Word32
deleted = 0x1

isDeleted :: Flags -> Bool
isDeleted (Flags w) = (w .&. deleted) == deleted
