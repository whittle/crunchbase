{-# LANGUAGE OverloadedStrings #-}
-- | Querying for a person takes a PersonPermalink, which can be
--   obtained from a search.
module Data.API.CrunchBase.PersonQuery
       ( PersonQuery(..)
       , PersonPermalink(..)
       ) where

import Data.API.CrunchBase.Query
import qualified Data.Text as T

newtype PersonPermalink = PersonPermalink T.Text deriving (Eq, Show)

newtype PersonQuery = PersonQuery PersonPermalink deriving (Eq, Show)

instance Query PersonQuery where
  toPathSegments (PersonQuery (PersonPermalink permalink)) =
    ["v", "1", "person", permalink `T.append` ".js"]

  toQueryItems _ = []
