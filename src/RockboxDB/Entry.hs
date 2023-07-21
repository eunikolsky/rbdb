{-# LANGUAGE RecordWildCards #-}

module RockboxDB.Entry
  ( Entry(..)
  , parser
  ) where

import Data.IntMap ((!?))
import Data.Text.Lazy qualified as TL
import Data.Time.Clock
import Numeric.Natural
import RockboxDB.IndexEntry qualified as IndexEntry
import RockboxDB.IndexEntry.Flags qualified as IndexEntry (Flags)
import RockboxDB.Prelude
import RockboxDB.TagFile.Filename qualified as Filename (getFilename)
import RockboxDB.TagFile.Filename qualified as TagFile (Filenames(..))

-- | Parsed valid rockbox database entry.
data Entry = Entry
  { filePath :: FilePath
  , duration :: NominalDiffTime
  , playCount :: Natural

  , playTime :: NominalDiffTime
  -- ^ total play time of this file; note that this is not played duration within
  -- the file — it can be higher than `duration`, apparently in cases when you
  -- listen to the (parts of the) file multiple times
  -- "This field does not work as per the others, but is used solely in the autoscore calculation"
  -- see the "Supported Tag Fields" table at
  -- https://www.rockbox.org/wiki/DataBase#tagnavi.config_v2.0_Syntax

  , progress :: Double
  -- ^ played progress, restricted to `[0; 1]`;
  -- this is a derived field: `playTime / duration`, no more than `1`;
  -- a simplified version of rockbox's autoscore: `100*playtime/length/playcount`
  -- given that bigger `playTime` doesn't necessarily mean playing later in the
  -- file (it can mean replaying the beginning again and again), this field is
  -- only an approximation of the real played progress

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
    , " (", show duration
    , ", ", show @Int . round $ progress * 100, "% played): "
    , show playCount, " plays"
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
    let duration = msToLength $ IndexEntry.lengthMs ie
    let playTime = msToLength $ IndexEntry.playTimeMs ie
    case filenameMap !? fromIntegral filenameOffset of
      Just filename -> Just $ Entry
        { filePath = TL.unpack . Filename.getFilename $ filename
        , duration
        , playCount = fromIntegral $ IndexEntry.playCount ie
        , playTime
        , playOrder = fromIntegral $ IndexEntry.lastPlayed ie
        --, modTime = IndexEntry.mtime ie
        , lastOffset = IndexEntry.lastOffset ie
        , lastElapsed = IndexEntry.lastElapsed ie
        , flags = IndexEntry.flags ie

        , progress = realToFrac (playTime / duration) `noMoreThan` 1
        }

      Nothing -> fail $ "Can't find filename at offset " <> show filenameOffset

msToLength :: Word32 -> NominalDiffTime
msToLength = secondsToNominalDiffTime . (/ 1e3) . fromIntegral

noMoreThan :: Ord a => a -> a -> a
noMoreThan = min
