{-# LANGUAGE ApplicativeDo, RecordWildCards #-}

module Config
  ( Config(..)
  , UseColor(..)
  , parser
  ) where

import Data.Char
import Data.List (intercalate)
import Options.Applicative
import RockboxDB as Database

-- | Program configuration parsed from command arguments.
data Config = Config
  { databaseDir :: !DatabaseDir
  , showOnlyFilenames :: !Bool
  , useColor :: !UseColor
  }

data UseColor
  = Yes
  | No
  | Auto
  -- ^ determine automatically based on whether the output is a TTY, default
  -- for unix tools
  deriving stock (Show, Bounded, Enum)

readUseColor :: String -> Maybe UseColor
readUseColor "yes" = Just Yes
readUseColor "no" = Just No
readUseColor "auto" = Just Auto
readUseColor _ = Nothing

parser :: Parser Config
parser = do
  -- the order of options in the generated help is based on the order here

  showOnlyFilenames <- flag False True
    ( short 'f'
    <> long "filename-only"
    -- showDefault doesn't print the default in a flag
    <> help "Show only filenames (default: false)"
    )

  useColor <- option (maybeReader readUseColor)
    ( long "color"
    <> help
      ("Show colorized output: "
      <> (fmap toLower . intercalate "|" $ show <$> enumerate @UseColor)
      )
    <> value Auto
    <> showDefaultWith (fmap toLower . show)
    )

  databaseDir <- DatabaseDir <$> strArgument
    ( metavar "ROCKBOX_PATH"
    <> help "Path to the rockbox database directory (with `database_*.tcd`)"
    )

  pure Config {..}

enumerate :: (Bounded a, Enum a) => [a]
enumerate = [minBound..maxBound]
