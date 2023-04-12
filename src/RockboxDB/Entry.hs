{-# LANGUAGE RecordWildCards #-}

module RockboxDB.Entry
  ( Entry(..)
  , parser
  ) where

import Data.IntMap ((!?))
import Data.Text qualified as T
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
  -- ^ unclear what it means
  -- "This field does not work as per the others, but is used solely in the autoscore calculation"
  -- see the "Supported Tag Fields" table at
  -- https://www.rockbox.org/wiki/DataBase#tagnavi.config_v2.0_Syntax

  , playOrder :: Natural
  -- ^ `lastPlayed` is play order, higher number is more recent; this meaning is
  -- based on the "Example 3 - Podcasts, Old and New" at
  -- https://www.rockbox.org/wiki/DataBase#Examples

  -- TODO modtime decodes into years around 2030, which is not correct
  -- , modTime :: Word32

  , lastOffset :: Word32
  -- ^ "Last offset into the file for automatic resume"
  -- https://www.rockbox.org/wiki/TagcacheDBFormat
  , lastElapsed :: Word32
  -- ^ this has to do with "time-based resume and playback start"
  -- source: https://git.rockbox.org/cgit/rockbox.git/commit/?id=31b712286721dd606940c7b557d03e3f714b9604

  , flags :: IndexEntry.Flags -- FIXME more suitable type
  }

instance Show Entry where
  show Entry{..} = mconcat
    [ "File ", filePath
    , ": ", show playCount, " plays"
    , ", playTime=", show playTime
    , ", playOrder=", show playOrder
    --, ", modTime=0x", showHex modTime ""
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
        , playOrder = fromIntegral $ IndexEntry.lastPlayed ie
        --, modTime = IndexEntry.mtime ie
        , lastOffset = IndexEntry.lastOffset ie
        , lastElapsed = IndexEntry.lastElapsed ie
        , flags = IndexEntry.flags ie
        }

      Nothing -> fail $ "Can't find filename at offset " <> show filenameOffset
