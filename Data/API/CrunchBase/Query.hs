-- | This module exports the definition of the Query typeclass
module Data.API.CrunchBase.Query
       ( -- * Query typeclass
         Query(..)
       ) where

import Data.Text (Text)
import Data.ByteString (ByteString)
import qualified Network.HTTP.Types as H
import Blaze.ByteString.Builder (toByteString)

-- | The minimum implementation of a Query is toPathSegments and
--   toQueryItems.
class Query q where
  toPathSegments :: q -> [Text]
  toQueryItems :: q -> H.Query

  toPath :: q -> ByteString
  toPath = toByteString . H.encodePathSegments . toPathSegments

  toQueryStr :: q -> H.Ascii
  toQueryStr = H.renderQuery True . toQueryItems
