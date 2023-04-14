module RockboxDB.IndexEntry.Flags
  ( Flags
  , isDeleted
  , mkFlags
  ) where

import Data.Bits
import RockboxDB.Prelude
import Text.Printf

-- https://www.rockbox.org/wiki/TagcacheDBFormat#Flags
newtype Flags = Flags Word32
  deriving newtype (Show)

deleted :: Word32
deleted = 0x1

isDeleted :: Flags -> Bool
isDeleted (Flags w) = (w .&. deleted) == deleted

-- | Smart constructor for `Flags`. Returns an error if the value is invalid.
mkFlags :: Word32 -> Either String Flags
mkFlags w = if w <= maxValue then Right (Flags w) else Left err
  where
    maxValue = 0b0001_1111
    err = printf "Unexpected flags value (max value is %#x): %#x" maxValue w
