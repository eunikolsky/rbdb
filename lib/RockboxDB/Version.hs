module RockboxDB.Version
  ( Version(..)
  , databaseFileIndexes
  , versionParser
  ) where

import RockboxDB.Prelude

-- | Database versions that can be parsed by the tool.
data Version
  = DB3_15
  -- ^ Database created by rockbox 3.15. Version bumped in
  -- https://git.rockbox.org/cgit/rockbox.git/commit/?id=31b712286721dd606940c7b557d03e3f714b9604
  | DBNext
  -- ^ Database created by the current development version of rockbox
  -- (not released yet). Version bumped in
  -- https://git.rockbox.org/cgit/rockbox.git/commit/?id=fcb9c068526cf1e565a05e6b6ccc50dc3e5ba0d5
  deriving stock Eq

-- | Indexes of database filenames (e.g. `0` for `database_0.tcd`) that are used
-- to calculate the expected data size.
databaseFileIndexes :: Version -> [Int]
databaseFileIndexes version = [0..3] <> [5..8] <> next
  where
    next | version == DBNext = [12]
         | otherwise = mempty

versionParser :: Parser Version
versionParser = do
  version <- choice
    [ DB3_15 <$ single 0x0f
    , DBNext <$ single 0x10
    ] <?> "database version"
  _magic <- string "\x48\x43\x54"
  pure version
