module Main (main) where

import Data.ByteString qualified as BS
import Data.List.NonEmpty qualified as NE
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

showErrorBundle :: (Show e, Show (Token s)) => ParseErrorBundle s e -> String
showErrorBundle ParseErrorBundle { bundleErrors } =
  unlines (show <$> NE.toList bundleErrors)
