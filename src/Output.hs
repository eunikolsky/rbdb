{-# LANGUAGE FlexibleContexts #-}

module Output
  ( printPodcast
  , showErrorBundle
  ) where

import Config
import Data.List (intercalate)
import Data.List.NonEmpty qualified as NE
import Data.Set qualified as Set
import Numeric
import RockboxDB.Entry as Entry
import RockboxDB.Prelude
import System.Console.ANSI
import System.FilePath

printPodcast :: Config -> Entry -> IO ()
printPodcast Config { showOnlyFilenames } Entry { filePath, progress, playCount } = mapM_ putStr $
  [colorFilePath filePath]
  -- TODO is there a cleaner syntax for this?
  <> (if showOnlyFilenames then mempty else
    [ ": "
    , coloredProgress
    , ", "
    , show playCount
    , " plays"
    ])
  <> ["\n"]

  where
    progressPercent = round $ progress * 100
    coloredProgress = progressColor $ show @Int progressPercent <> "%"
    progressColor = if
      | progressPercent == 100 -> brightGreen
      | progressPercent >= 80 -> green
      | otherwise -> brightRed

    green = withColor (Dull, Green)
    brightGreen = withColor (Vivid, Green)
    brightRed = withColor (Vivid, Red)

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

    blue = withColor (Dull, Blue)
    yellow = withColor (Dull, Yellow)

withColor :: (ColorIntensity, Color) -> String -> String
withColor (intensity, color) s = setSGRCode [SetColor Foreground intensity color] <> s <> setSGRCode []

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
