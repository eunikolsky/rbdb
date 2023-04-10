module Main (main) where

import Data.ByteString qualified as BS
import Data.List (intercalate)
import Data.List.NonEmpty qualified as NE
import Data.Set qualified as Set
import Data.Void
import Numeric
import RockboxDB
import System.Environment
import System.Exit
import Text.Megaparsec

main :: IO ()
main = getIndexFilepath >>= parseDatabase >>= printDatabase

getIndexFilepath :: IO FilePath
getIndexFilepath = do
  args <- getArgs
  case args of
    [fp] -> pure fp
    _ -> die "Provide the path to the rockbox database index file (database_idx.tcd)"

parseDatabase :: FilePath -> IO Database
parseDatabase fp = do
  bytes <- BS.readFile fp
  case runParser dbParser fp bytes of
    Right db -> pure db
    -- errorBundlePretty prints the data where the failure occurs, which doesn't
    -- work well with binary data by default; hex dump would be much better
    -- https://github.com/mrkkrp/megaparsec/issues/465
    Left errBundle -> die $ showErrorBundle errBundle

printDatabase :: Database -> IO ()
printDatabase = print

showErrorBundle :: Show (Token s) => ParseErrorBundle s Void -> String
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
