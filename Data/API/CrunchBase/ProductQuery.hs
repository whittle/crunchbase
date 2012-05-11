{-# LANGUAGE OverloadedStrings #-}
-- | Querying for a product takes a ProductPermalink, which can be
--   obtained from a search.
module Data.API.CrunchBase.ProductQuery
       ( ProductQuery(..)
       , ProductPermalink(..)
       ) where

import Data.API.CrunchBase.Query
import qualified Data.Text as T
import Data.Aeson (FromJSON(..), Value(..))

newtype ProductQuery = ProductQuery ProductPermalink deriving (Eq, Show, Read)

instance Query ProductQuery where
  toPathSegments (ProductQuery (ProductPermalink permalink)) =
    ["v", "1", "product", permalink `T.append` ".js"]

  toQueryItems _ = []

newtype ProductPermalink = ProductPermalink T.Text deriving (Eq, Show, Read)

instance FromJSON ProductPermalink where
  parseJSON (String s) = return . ProductPermalink $ s
