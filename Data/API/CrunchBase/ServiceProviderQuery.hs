{-# LANGUAGE OverloadedStrings #-}
module Data.API.CrunchBase.ServiceProviderQuery
       ( ServiceProviderQuery(..)
       , ServiceProviderPermalink(..)
       ) where

import Data.API.CrunchBase.Query
import qualified Data.Text as T

newtype ServiceProviderQuery =
  ServiceProviderQuery ServiceProviderPermalink
  deriving (Eq, Show)

newtype ServiceProviderPermalink =
  ServiceProviderPermalink T.Text
  deriving (Eq, Show)

instance Query ServiceProviderQuery where
  toPathSegments (ServiceProviderQuery
                  (ServiceProviderPermalink permalink)) =
    [ "v"
    , "1"
    , "service-provider"
    , permalink `T.append` ".js"
    ]

  toQueryItems _ = []