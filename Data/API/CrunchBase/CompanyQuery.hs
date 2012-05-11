{-# LANGUAGE OverloadedStrings #-}
-- | Querying for a company takes a CompanyPermalink, which can be
--   obtained from a search.
module Data.API.CrunchBase.CompanyQuery
       ( CompanyQuery(..)
       , CompanyPermalink(..)
       ) where

import Data.API.CrunchBase.Query
import qualified Data.Text as T
import Data.Aeson (FromJSON(..), Value(..))

newtype CompanyQuery = CompanyQuery CompanyPermalink deriving (Eq, Show, Read)

instance Query CompanyQuery where
  toPathSegments (CompanyQuery (CompanyPermalink permalink)) =
    ["v", "1", "company", permalink `T.append` ".js"]

  toQueryItems _ = []

newtype CompanyPermalink = CompanyPermalink T.Text deriving (Eq, Show, Read)

instance FromJSON CompanyPermalink where
  parseJSON (String s) = return . CompanyPermalink $ s
