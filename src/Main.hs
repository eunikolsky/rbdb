module Main (main) where

import Data.List (intercalate, isPrefixOf)
import Data.List.NonEmpty qualified as NE
import Data.Set qualified as Set
import Data.Void
import Numeric
import RockboxDB as Database
import RockboxDB.Entry as Entry
import RockboxDB.Prelude
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
-- TODO color podcast and episode?
printPodcasts = mapM_ print . filter isPodcast . Database.validEntries
  where isPodcast = ("/podcasts" `isPrefixOf`) . Entry.filePath

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
