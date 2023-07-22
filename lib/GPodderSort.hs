module GPodderSort
  ( gPodderTitleSortKey
  ) where

import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T

gPodderTitleSortKey :: Text -> Text
gPodderTitleSortKey = translate replacements . removePrefix "the " . T.toLower

removePrefix :: Text -> Text -> Text
removePrefix prefix text = fromMaybe text $ T.stripPrefix prefix text

-- | Replaces the first character in the pair with the second character, for
-- each pair.
translate :: [(Char, Char)] -> Text -> Text
translate tr = T.map $ \c -> fromMaybe c $ lookup c tr

-- https://github.com/gpodder/gpodder/blob/74d73231d118caa52661fb16de870e971f6b8164/src/gpodder/model.py#L921
replacements :: [(Char, Char)]
replacements = [('ö', 'o'), ('ü', 'u'), ('ä', 'a')]
