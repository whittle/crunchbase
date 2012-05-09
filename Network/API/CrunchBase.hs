{-# LANGUAGE OverloadedStrings #-}
-- | This module is used to run queries on the API. The queries
--   themselves are constructed elsewhere.
module Network.API.CrunchBase
       ( sendRequest
       , mkRequest
       , SearchQuery
       , CompanyQuery
       , Company
       , PersonQuery
       , Person
       , FinancialOrganizationQuery
       , ProductQuery
       , Product
       , ServiceProviderQuery
       ) where

import Data.API.CrunchBase.Query
import Data.API.CrunchBase.SearchQuery
import Data.API.CrunchBase.CompanyQuery
import Data.API.CrunchBase.CompanyResponse
import Data.API.CrunchBase.PersonQuery
import Data.API.CrunchBase.PersonResponse
import Data.API.CrunchBase.FinancialOrganizationQuery
import Data.API.CrunchBase.ProductQuery
import Data.API.CrunchBase.Product
import Data.API.CrunchBase.ServiceProviderQuery

import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy as B
import Data.Aeson (decode)

-- | Get a Company using a CompanyPermalink.
getCompany :: CompanyPermalink -> IO (Maybe Company)
getCompany = fmap (decode . responseBody) . sendRequest . CompanyQuery

-- | Get a Person using a PersonPermalink.
getPerson :: PersonPermalink -> IO (Maybe Person)
getPerson = fmap (decode . responseBody) . sendRequest . PersonQuery

-- | Get a Product using a ProductPermalink.
getProduct :: ProductPermalink -> IO (Maybe Product)
getProduct = fmap (decode . responseBody) . sendRequest . ProductQuery

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
