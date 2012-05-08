{-# LANGUAGE OverloadedStrings #-}
-- | This module is used to run queries on the API. The queries
--   themselves are constructed elsewhere.
module Network.API.CrunchBase
       ( sendRequest
       , mkRequest
       , CompanyQuery(..)
       , PersonQuery(..)
       ) where

import Data.API.CrunchBase.Query
import Data.API.CrunchBase.CompanyQuery
import Data.API.CrunchBase.PersonQuery

import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy as B

-- | This function takes a member of the Query typeclass and returns
--   an IO action which will fetch a Response.
sendRequest :: (Query q) => q -> IO (Response B.ByteString)
sendRequest query = withManager $ httpLbs (mkRequest query)

-- | This function creates an HTTP request from any type in the Query
--   class.
mkRequest :: (Query q) => q -> (Request m)
mkRequest query = def
  { method = "GET"
  , host = "api.crunchbase.com"
  , path = toPath query
  , queryString = toQueryStr query
  }
