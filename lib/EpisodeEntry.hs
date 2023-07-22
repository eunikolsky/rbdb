module EpisodeEntry
  ( EpisodeEntry(..)
  , EpisodePath(..)
  , mkEpisodeEntry
  ) where

import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as TL
import GHC.Stack (HasCallStack)
import RockboxDB.Entry qualified as RockboxDB
import System.FilePath

-- `String`s because it seems to be a waste of resources to convert from the
-- right `Text` to the necessary `FilePath`, to `Text` to store here, then to
-- `String` again to output with colors.
data EpisodePath = EpisodePath
  { root :: String
  , podcast :: String
  , episode :: String
  }

-- | gPodder sync-based podcast episode entry, which is the rockbox's
-- `RockboxDB.Entry` with the `filePath` parsed into podcast and episode parts.
data EpisodeEntry = EpisodeEntry
  { path :: !EpisodePath
  , entry :: !RockboxDB.Entry
  }

-- | Parses a filepath into `EpisodePath`.
mkEpisodePath :: HasCallStack => Text -> EpisodePath
mkEpisodePath filePath =
  case splitEpisodePath $ TL.unpack filePath of
    Just (root, podcast, episode) -> EpisodePath root podcast episode
    Nothing -> error $ "Failed to parse filepath: " <> TL.unpack filePath

-- | Creates an `EpisodeEntry` from a `RockboxDB.Entry`.
mkEpisodeEntry :: RockboxDB.Entry -> EpisodeEntry
mkEpisodeEntry entry = EpisodeEntry
  { path = mkEpisodePath $ RockboxDB.filePath entry
  , entry
  }

-- | Splits the standard (for me) filepath like `/podcasts/podcast/episode.mp3` into
-- three parts: `("/podcasts", "podcast", "episode.mp3")`; returns `Nothing`
-- if there is no root in the filepath.
splitEpisodePath :: FilePath -> Maybe (String, String, String)
splitEpisodePath fp = case reverse . splitDirectories $ fp of
  -- `reverse` seems to be the simplest way to pick the last two items with
  -- the rest in front as the `rest`
  (episode : podcast : rest) -> Just (joinPath $ reverse rest, podcast, episode)
  _ -> Nothing
