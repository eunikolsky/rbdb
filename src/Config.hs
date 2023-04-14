module Config
  ( Config(..)
  ) where

import RockboxDB as Database

-- | Program configuration parsed from command arguments.
newtype Config = Config
  { databaseDir :: DatabaseDir }
