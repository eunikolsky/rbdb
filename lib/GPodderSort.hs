module GPodderSort
  ( gPodderTitleSortKey
  ) where

import Data.Text (Text)
import Data.Text qualified as T

gPodderTitleSortKey :: Text -> Text
gPodderTitleSortKey = T.toLower
