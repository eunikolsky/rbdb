module RockboxDB.TagFile.Filename
  ( Filename(..)
  , Filenames(..)
  , parser
  ) where

import Control.Monad
import Data.ByteString qualified as BS
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import RockboxDB.Prelude

newtype Filename = Filename { getFilename :: Text }

-- | Parsed tag file `database_4.tcd` with filenames. The map maps the entry's
-- byte offset to the corresponding filename.
newtype Filenames = Filenames (IntMap Filename)

parser :: Parser Filenames
parser = do
  -- TODO dedup
  _magic <- string "\x0f\x48\x43\x54"
  _dataSize <- word32
  numEntries <- fromIntegral <$> word32

  indexedTexts <- count numEntries $ do
    offset <- getOffset
    bytesLength <- subtract 1 . fromIntegral <$> word32
    _masterIndex <- word32
    bytes <- count bytesLength word8
    void $ char 0

    pure (offset, Filename . TE.decodeUtf8 . BS.pack $ bytes)

  pure . Filenames $ IntMap.fromList indexedTexts
