module Config
  ( Config(..)
  , parser
  ) where

import Options.Applicative
import RockboxDB as Database

-- | Program configuration parsed from command arguments.
newtype Config = Config
  { databaseDir :: DatabaseDir }

parser :: Parser Config
parser = Config . DatabaseDir
  <$> strArgument
      ( metavar "ROCKBOX_PATH"
      <> help "Path to the rockbox database directory (with `database_*.tcd`)"
      )
