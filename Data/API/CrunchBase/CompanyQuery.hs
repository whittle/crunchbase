{-# LANGUAGE OverloadedStrings #-}
-- | Querying for a company takes a CompanyPermalink, which can be
--   obtained from a search.
module Data.API.CrunchBase.CompanyQuery
       ( CompanyQuery(..)
       , CompanyPermalink(..)
       ) where

import Data.API.CrunchBase.Query
import qualified Data.Text as T

newtype CompanyPermalink = CompanyPermalink T.Text deriving (Eq, Show)

newtype CompanyQuery = CompanyQuery CompanyPermalink deriving (Eq, Show)

instance Query CompanyQuery where
  toPathSegments (CompanyQuery (CompanyPermalink permalink)) =
    ["v", "1", "company", permalink `T.append` ".js"]

  toQueryItems _ = []
