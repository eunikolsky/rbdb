{-# LANGUAGE RecordWildCards #-}

module RockboxDB.Entry
  ( Entry(..)
  , parser
  ) where

import Data.IntMap ((!?))
import Data.Text qualified as T
import Numeric
import Numeric.Natural
import RockboxDB.IndexEntry qualified as IndexEntry
import RockboxDB.IndexEntry.Flags qualified as IndexEntry (Flags)
import RockboxDB.Prelude
import RockboxDB.TagFile.Filename qualified as Filename (getFilename)
import RockboxDB.TagFile.Filename qualified as TagFile (Filenames(..))

-- | Parsed valid rockbox database entry.
data Entry = Entry
  { filePath :: FilePath
  , playCount :: Natural
  , playTime :: Natural
  , lastPlayed :: Word32 -- FIXME decode into a time
  , modTime :: Word32 -- FIXME decode into a time
  , lastOffset :: Word32
  , lastElapsed :: Word32
  , flags :: IndexEntry.Flags -- FIXME more suitable type
  }

instance Show Entry where
  show Entry{..} = mconcat
    [ "File ", filePath
    , ": ", show playCount, " plays"
    , ", playTime=", show playTime
    , ", lastPlayed=", show lastPlayed
    , ", modTime=0x", showHex modTime ""
    , ", lastOffset=", show lastOffset
    , ", lastElapsed=", show lastElapsed
    , ", flags=", show flags
    ]

-- returns Nothing if the entry is invalid (was deleted)
parser :: TagFile.Filenames -> Parser (Maybe Entry)
parser (TagFile.Filenames filenameMap) = do
  ie <- IndexEntry.parser
  pure $ do
    filenameOffset <- IndexEntry.maybeFilenameOffset ie
    case filenameMap !? fromIntegral filenameOffset of
      Just filename -> Just $ Entry
        { filePath = T.unpack . Filename.getFilename $ filename
        , playCount = fromIntegral $ IndexEntry.playCount ie
        , playTime = fromIntegral $ IndexEntry.playTime ie
        , lastPlayed = IndexEntry.lastPlayed ie
        , modTime = IndexEntry.mtime ie
        , lastOffset = IndexEntry.lastOffset ie
        , lastElapsed = IndexEntry.lastElapsed ie
        , flags = IndexEntry.flags ie
        }

      Nothing -> fail $ "Can't find filename at offset " <> show filenameOffset
