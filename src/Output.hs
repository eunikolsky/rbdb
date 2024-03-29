{-# LANGUAGE FlexibleContexts #-}

module Output
  ( printPodcast
  , showErrorBundle
  ) where

import Config
import Config qualified as UseColor (UseColor(..))
import Control.Monad (join)
import Control.Monad.Reader
import Data.List (intercalate, singleton)
import Data.List.NonEmpty qualified as NE
import Data.Set qualified as Set
import Data.Text.Lazy qualified as TL
import EpisodeEntry
import Numeric
import RockboxDB.Entry as Entry
import RockboxDB.Prelude hiding (showErrorItem)
import System.Console.ANSI
import System.FilePath
import System.IO

printPodcast :: Config -> EpisodeEntry -> IO ()
printPodcast
  Config { outputConfig = NormalOutput NormalOutputConfig { showOnlyFilenames, useColor } }
  episodeEntry@EpisodeEntry { path = filePath, entry = Entry { progress, playCount } }
  = do
    supportsColor <- determineColorSupport useColor
    mapM_ putStrLn . flip runReader supportsColor $ do
      let colorProgress = getProgressColor progress
      cfilePath <- colorFilePath colorProgress filePath
      -- TODO is there a cleaner syntax for this?
      crest <- if showOnlyFilenames then pure [] else do
        cprogress <- colorProgress $ toUserProgress progress
        pure
          [ ": "
          , cprogress
          , ", "
          , show playCount
          , " plays, file "
          , maybe " progress unknown" ((<> " played") . toUserProgress) $ fileProgress episodeEntry
          ]
      pure . singleton . join $ cfilePath : crest

printPodcast
  Config { outputConfig = Dump }
  episodeEntry@EpisodeEntry { entry }
  = putStrLn . mconcat $
    [ show entry
    , ", file progress"
    ] <> maybe [" unknown"] (\p -> ["=", show p]) (fileProgress episodeEntry)

type Colorizer = String -> Reader SupportsColor String

getProgressColor :: Double -> Colorizer
getProgressColor progress
  | progress >= 0.96 = brightGreen
  | progress >= 0.8 = green
  | otherwise = brightRed

  where
    brightGreen = withColor (Vivid, Green)
    green = withColor (Dull, Green)
    brightRed = withColor (Vivid, Red)

colorFilePath :: Colorizer -> EpisodePath -> Reader SupportsColor String
colorFilePath colorize (EpisodePath root podcast episode) = do
  cpodcast <- colorize $ TL.unpack podcast
  cepisode <- colorize episode
  cpathsep <- withColor (Vivid, Magenta) $ [pathSeparator]
  pure $ intercalate cpathsep [root, cpodcast, cepisode]

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
