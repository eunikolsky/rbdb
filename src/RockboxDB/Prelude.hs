{-|
Description : Custom prelude for rockbox database parsers

This is an idea from the "Production Haskell" book by Matt Parsons.
-}
module RockboxDB.Prelude
  ( ByteString
  , Parser

  , word32

  , module Data.Void
  , module Data.Word
  , module Text.Megaparsec
  ) where

import Data.ByteString (ByteString)
import Data.Void
import Data.Word
import Text.Megaparsec
import Text.Megaparsec.Byte.Binary

-- | The parser type for binary rockbox databases.
type Parser = Parsec Void ByteString

-- | Parses 4 bytes as a little-endian word (for SanDisk Sansa Clip+).
word32 :: Parser Word32
word32 = word32le
