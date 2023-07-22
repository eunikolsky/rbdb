module GPodderSort
  ( gPodderTitleSortKey
  ) where

import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T

gPodderTitleSortKey :: Text -> Text
gPodderTitleSortKey = removePrefix "the " . T.toLower

removePrefix :: Text -> Text -> Text
removePrefix prefix text = fromMaybe text $ T.stripPrefix prefix text
