module Main (main) where

import Data.List (isPrefixOf, sortOn)
import Output
import RockboxDB as Database
import RockboxDB.Entry as Entry
import System.Environment
import System.Exit

main :: IO ()
main = getIndexFilepath >>= parseDatabase >>= printPodcasts

getIndexFilepath :: IO DatabaseDir
getIndexFilepath = do
  args <- getArgs
  case args of
    [fp] -> pure $ DatabaseDir fp
    _ -> die "Provide the path to the rockbox database directory (with `database_*.tcd`)"

parseDatabase :: DatabaseDir -> IO Database
parseDatabase dir = do
  eitherDB <- Database.parse dir
  case eitherDB of
    Right db -> pure db
    -- errorBundlePretty prints the data where the failure occurs, which doesn't
    -- work well with binary data by default; hex dump would be much better
    -- https://github.com/mrkkrp/megaparsec/issues/465
    Left errBundle -> die $ showErrorBundle errBundle

printPodcasts :: Database -> IO ()
printPodcasts
  = mapM_ printPodcast
  . lessRecentFirst
  . filter (\e -> isPlayed e && isPodcast e)
  . Database.validEntries

  where
    isPodcast = ("/podcasts" `isPrefixOf`) . Entry.filePath
    -- note: this doesn't necessarily mean that a file has been played entirely
    isPlayed = (> 0) . Entry.playCount
    lessRecentFirst = sortOn Entry.playOrder
