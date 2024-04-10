module RockboxDB.Version
  ( Version(..)
  , numFiles
  , versionParser
  ) where

import RockboxDB.Prelude

-- | Database versions that can be parsed by the tool.
data Version
  = DB3_15
  -- ^ Database created by rockbox 3.15. Version bumped in
  -- https://git.rockbox.org/cgit/rockbox.git/commit/?id=31b712286721dd606940c7b557d03e3f714b9604

numFiles :: Version -> Int
numFiles DB3_15 = 9

versionParser :: Parser Version
versionParser = do
  version <- DB3_15 <$ single 0x0f
  _magic <- string "\x48\x43\x54"
  pure version
