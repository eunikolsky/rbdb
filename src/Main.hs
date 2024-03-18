module Main (main) where

import Config
import Data.List (sortOn)
import Data.Text.Lazy qualified as TL
import Data.Version
import EpisodeEntry
import EpisodeEntry qualified as EpisodePath (EpisodePath(..))
import GPodderSort
import Options.Applicative
import Output
import Paths_rbdb (version)
import RockboxDB as Database
import RockboxDB.Entry as Entry
import System.Exit
import System.FilePath (takeDirectory)

main :: IO ()
main = do
  config <- parseConfig
  db <- parseDatabase config
  printPodcasts config db

parseConfig :: IO Config
parseConfig = execParser opts
  where
    opts = info (Config.parser <**> versioner <**> helper)
      ( fullDesc
      <> header "Print played podcast episodes from the rockbox database"
      )

    versioner = infoOption (showVersion version)
      (long "version" <> help "Show version" <> hidden)

parseDatabase :: Config -> IO Database
parseDatabase Config { databaseDir = dir } = do
  eitherDB <- Database.parse dir
  case eitherDB of
    Right db -> pure db
    -- errorBundlePretty prints the data where the failure occurs, which doesn't
    -- work well with binary data by default; hex dump would be much better
    -- https://github.com/mrkkrp/megaparsec/issues/465
    Left errBundle -> die $ showErrorBundle errBundle

printPodcasts :: Config -> Database -> IO ()
printPodcasts config db = do
  entries <-
    traverse (mkEpisodeEntry mountDir)
    . filter (\e -> all ($ e) [hasNonTrivialProgress, isPlayed, isPodcast])
    . Database.validEntries
    $ db
  mapM_ (printPodcast config) $ sortedByPodcast entries

  where
    isPodcast = ("/podcasts" `TL.isPrefixOf`) . Entry.filePath
    -- note: this doesn't necessarily mean that a file has been played entirely
    isPlayed = (> 0) . Entry.playCount
    hasNonTrivialProgress = (> 0.03) . Entry.progress
    sortedByPodcast = sortOn (
        (\path -> (gPodderTitleSortKey . EpisodePath.podcast $ path, episode path))
        . EpisodeEntry.path
      )
    mountDir = takeDirectory . takeDirectory . (<> "/") . getDatabaseDir . databaseDir $ config
