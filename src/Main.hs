{-# LANGUAGE FlexibleContexts #-}

module Main (main) where

import Data.List (intercalate, isPrefixOf, sortOn)
import Data.List.NonEmpty qualified as NE
import Data.Set qualified as Set
import Data.Void
import Numeric
import RockboxDB as Database
import RockboxDB.Entry as Entry
import RockboxDB.Prelude
import System.Console.ANSI
import System.Environment
import System.Exit
import System.FilePath

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

printPodcast :: Entry -> IO ()
printPodcast Entry { filePath, progress, playCount } = mapM_ putStr
  [ colorFilePath filePath
  , ": "
  , coloredProgress
  , ", "
  , show playCount
  , " plays\n"
  ]

  where
    progressPercent = round $ progress * 100
    coloredProgress = progressColor $ show @Int progressPercent <> "%"
    progressColor = if
      | progressPercent == 100 -> brightGreen
      | progressPercent >= 80 -> green
      | otherwise -> brightRed

    green s = setSGRCode [SetColor Foreground Dull Green] <> s <> setSGRCode []
    brightGreen s = setSGRCode [SetColor Foreground Vivid Green] <> s <> setSGRCode []
    brightRed s = setSGRCode [SetColor Foreground Vivid Red] <> s <> setSGRCode []

colorFilePath :: FilePath -> String
colorFilePath fp = case splitEpisodePath of
  Just (root, podcast, episode) -> intercalate [pathSeparator]
    [root, blue podcast, yellow episode]
  Nothing -> fp

  where
    -- | Splits the standard (for me) filepath like `/podcasts/podcast/episode.mp3` into
    -- three parts: `("/podcasts", "podcast", "episode.mp3")`; returns `Nothing`
    -- if there is no root in the filepath.
    splitEpisodePath = case reverse $ splitDirectories fp of
      -- `reverse` seems to be the simplest way to pick the last two items with
      -- the rest in front as the `rest`
      (episode : podcast : rest) -> Just (joinPath $ reverse rest, podcast, episode)
      _ -> Nothing

    blue s = setSGRCode [SetColor Foreground Dull Blue] <> s <> setSGRCode []
    yellow s = setSGRCode [SetColor Foreground Dull Yellow] <> s <> setSGRCode []

showErrorBundle :: ParseError -> String
showErrorBundle ParseErrorBundle { bundleErrors } =
  unlines (showError <$> NE.toList bundleErrors)

  where
    showError (TrivialError pos maybeUnexpectedToken expectedTokens) = mconcat
      [ "Error at byte 0x" <> showHex pos "" <> " (" <> show pos <> "):\n  "
      , maybe "no unexpected token" showErrorItem maybeUnexpectedToken
      , "\n  expected tokens: "
      , intercalate "; " . fmap showErrorItem $ Set.toList expectedTokens
      ]
    showError (FancyError pos fancyErrors) = mconcat
      [ "Error at byte 0x" <> showHex pos "" <> " (" <> show pos <> "):\n  "
      , intercalate "; " . fmap showErrorFancy $ Set.toList fancyErrors
      ]

    showErrorItem (Tokens ts) = intercalate ", " $ show <$> NE.toList ts
    showErrorItem (Label cs) = NE.toList cs
    showErrorItem EndOfInput = "EndOfInput"

    showErrorFancy (ErrorFail s) = s
    showErrorFancy (ErrorIndentation {}) = error "Unexpected indentation error"
    showErrorFancy (ErrorCustom e) = absurd e
