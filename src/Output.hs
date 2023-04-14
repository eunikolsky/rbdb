{-# LANGUAGE FlexibleContexts #-}

module Output
  ( printPodcast
  , showErrorBundle
  ) where

import Config
import Config qualified as UseColor (UseColor(..))
import Control.Monad.Reader
import Data.List (intercalate, singleton)
import Data.List.NonEmpty qualified as NE
import Data.Set qualified as Set
import Numeric
import RockboxDB.Entry as Entry
import RockboxDB.Prelude
import System.Console.ANSI
import System.FilePath
import System.IO

printPodcast :: Config -> Entry -> IO ()
printPodcast
  Config { showOnlyFilenames, useColor }
  Entry { filePath, progress, playCount }
  = do
    supportsColor <- determineColorSupport useColor
    mapM_ putStrLn . flip runReader supportsColor $ do
      cfilePath <- colorFilePath filePath
      -- TODO is there a cleaner syntax for this?
      crest <- if showOnlyFilenames then pure [] else do
        cprogress <- colorProgress progress
        pure
          [ ": "
          , cprogress
          , ", "
          , show playCount
          , " plays"
          ]
      pure . singleton . join $ cfilePath : crest

colorProgress :: Double -> Reader SupportsColor String
colorProgress progress = progressColor $ show @Int progressPercent <> "%"
  where
    progressPercent = round $ progress * 100
    progressColor = if
      | progressPercent == 100 -> brightGreen
      | progressPercent >= 80 -> green
      | otherwise -> brightRed

    green = withColor (Dull, Green)
    brightRed = withColor (Vivid, Red)

colorFilePath :: FilePath -> Reader SupportsColor String
colorFilePath fp = case splitEpisodePath of
  Just (root, podcast, episode) -> do
    cpodcast <- blue podcast
    cepisode <- brightGreen episode
    pure $ intercalate [pathSeparator] [root, cpodcast, cepisode]
  Nothing -> pure fp

  where
    -- | Splits the standard (for me) filepath like `/podcasts/podcast/episode.mp3` into
    -- three parts: `("/podcasts", "podcast", "episode.mp3")`; returns `Nothing`
    -- if there is no root in the filepath.
    splitEpisodePath = case reverse $ splitDirectories fp of
      -- `reverse` seems to be the simplest way to pick the last two items with
      -- the rest in front as the `rest`
      (episode : podcast : rest) -> Just (joinPath $ reverse rest, podcast, episode)
      _ -> Nothing

    blue = withColor (Dull, Blue)

brightGreen :: String -> Reader SupportsColor String
brightGreen = withColor (Vivid, Green)

withColor :: (ColorIntensity, Color) -> String -> Reader SupportsColor String
withColor (intensity, color) s = do
  SupportsColor supportsColor <- ask
  pure $ if supportsColor
    then setSGRCode [SetColor Foreground intensity color] <> s <> setSGRCode []
    else s

newtype SupportsColor = SupportsColor Bool

determineColorSupport :: UseColor -> IO SupportsColor
determineColorSupport UseColor.Yes = pure $ SupportsColor True
determineColorSupport UseColor.No = pure $ SupportsColor False
determineColorSupport UseColor.Auto = SupportsColor <$> hSupportsANSIColor stdout

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
