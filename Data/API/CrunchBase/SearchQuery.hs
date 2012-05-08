{-# LANGUAGE OverloadedStrings #-}
-- | Searches currently only take a snippet of Text to search for.
module Data.API.CrunchBase.SearchQuery
       ( SearchQuery(..)
       ) where

import Data.API.CrunchBase.Query
import qualified Data.Text as T
import qualified Network.HTTP.Types as H

newtype SearchQuery = SearchQuery T.Text deriving (Eq, Show)

instance Query SearchQuery where
  toPathSegments _ = ["v", "1", "search.js"]

  toQueryItems (SearchQuery queryText) =
    H.queryTextToQuery [("query", Just queryText)]
