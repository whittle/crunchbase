{-# LANGUAGE OverloadedStrings #-}
-- | Querying for a service provider takes a ServiceProviderPermalink,
--   which can be obtained from a search.
module Data.API.CrunchBase.ServiceProviderQuery
       ( ServiceProviderQuery(..)
       , ServiceProviderPermalink(..)
       ) where

import Data.API.CrunchBase.Query
import qualified Data.Text as T
import Data.Aeson (FromJSON(..), Value(..))

newtype ServiceProviderQuery =
  ServiceProviderQuery ServiceProviderPermalink
  deriving (Eq, Show)

instance Query ServiceProviderQuery where
  toPathSegments (ServiceProviderQuery
                  (ServiceProviderPermalink permalink)) =
    ["v", "1", "service-provider", permalink `T.append` ".js"]

  toQueryItems _ = []

newtype ServiceProviderPermalink =
  ServiceProviderPermalink T.Text
  deriving (Eq, Show)

instance FromJSON ServiceProviderPermalink where
  parseJSON (String s) = return . ServiceProviderPermalink $ s
