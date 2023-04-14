module Main (main) where

import Config
import Data.List (isPrefixOf, sortOn)
import Options.Applicative
import Output
import RockboxDB as Database
import RockboxDB.Entry as Entry
import System.Exit

main :: IO ()
main = do
  config <- parseConfig
  db <- parseDatabase config
  printPodcasts config db

parseConfig :: IO Config
parseConfig = execParser opts
  where
    opts = info (Config.parser <**> helper)
      ( fullDesc
      <> header "Print played podcast episodes from the rockbox database"
      )

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
printPodcasts config
  = mapM_ (printPodcast config)
  . lessRecentFirst
  . filter (\e -> isPlayed e && isPodcast e)
  . Database.validEntries

  where
    isPodcast = ("/podcasts" `isPrefixOf`) . Entry.filePath
    -- note: this doesn't necessarily mean that a file has been played entirely
    isPlayed = (> 0) . Entry.playCount
    lessRecentFirst = sortOn Entry.playOrder
