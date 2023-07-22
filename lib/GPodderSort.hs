module GPodderSort
  ( gPodderTitleSortKey
  ) where

import Data.Maybe
import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as TL

-- | Returns the gPodder title sort key for the given podcast title; the key is
-- used to sort a list of podcasts.
--
-- Based on: <https://github.com/gpodder/gpodder/blob/74d73231d118caa52661fb16de870e971f6b8164/src/gpodder/model.py#L1094-L1096>
gPodderTitleSortKey :: Text -> Text
gPodderTitleSortKey = translate replacements . removePrefix "the " . TL.toLower

removePrefix :: Text -> Text -> Text
removePrefix prefix text = fromMaybe text $ TL.stripPrefix prefix text

-- | Replaces the first character in the pair with the second character, for
-- each pair.
translate :: [(Char, Char)] -> Text -> Text
translate tr = TL.map $ \c -> fromMaybe c $ lookup c tr

-- https://github.com/gpodder/gpodder/blob/74d73231d118caa52661fb16de870e971f6b8164/src/gpodder/model.py#L921
replacements :: [(Char, Char)]
replacements = [('ö', 'o'), ('ü', 'u'), ('ä', 'a')]
