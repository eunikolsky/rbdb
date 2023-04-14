{-# LANGUAGE ApplicativeDo, RecordWildCards #-}

module Config
  ( Config(..)
  , parser
  ) where

import Options.Applicative
import RockboxDB as Database

-- | Program configuration parsed from command arguments.
data Config = Config
  { databaseDir :: !DatabaseDir
  , showOnlyFilenames :: !Bool
  }

parser :: Parser Config
parser = do
  -- the order of options in the generated help is based on the order here
  showOnlyFilenames <- flag False True
      ( short 'f'
      <> long "filename-only"
      -- showDefault doesn't print the default in a flag
      <> help "Show only filenames (default: false)"
      )
  databaseDir <- DatabaseDir <$> strArgument
      ( metavar "ROCKBOX_PATH"
      <> help "Path to the rockbox database directory (with `database_*.tcd`)"
      )
  pure Config {..}
