module RockboxDB.TagFile.Filename
  ( Filename(..)
  , Filenames(..)
  , headerSize
  , parser
  ) where

import Control.Monad
import Data.ByteString.Lazy qualified as BSL
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Text.Lazy (Text)
import Encoding
import RockboxDB.Prelude
import RockboxDB.Version

newtype Filename = Filename { getFilename :: Text }

-- | Parsed tag file `database_4.tcd` with filenames. The map maps the entry's
-- byte offset to the corresponding filename.
newtype Filenames = Filenames (IntMap Filename)

parser :: Parser Filenames
parser = do
  -- TODO dedup
  _version <- versionParser
  _dataSize <- word32
  numEntries <- fromIntegral <$> word32

  indexedTexts <- count numEntries $ do
    offset <- getOffset
    bytesLength <- subtract 1 . fromIntegral <$> word32
    _masterIndex <- word32
    bytes <- count bytesLength word8
    void $ char 0

    pure (offset, Filename . decodeCesu8 . BSL.pack $ bytes)

  eof

  pure . Filenames $ IntMap.fromList indexedTexts

-- | Header size in bytes.
headerSize :: Num a => a
headerSize = 3 * 4
