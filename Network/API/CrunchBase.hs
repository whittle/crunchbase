{-# LANGUAGE OverloadedStrings #-}
-- | This module is used to run queries on the API. The queries
--   themselves are constructed elsewhere.
module Network.API.CrunchBase
       ( sendRequest
       , mkRequest
       , SearchQuery
       , SearchPage
       , SearchResult
       , CompanyQuery
       , Company
       , PersonQuery
       , Person
       , FinancialOrganizationQuery
       , FinancialOrganization
       , ProductQuery
       , Product
       , ServiceProviderQuery
       , ServiceProvider
       ) where

import Data.API.CrunchBase.Query
import Data.API.CrunchBase.SearchQuery
import Data.API.CrunchBase.SearchPage
import Data.API.CrunchBase.SearchResult
import Data.API.CrunchBase.CompanyQuery
import Data.API.CrunchBase.Company
import Data.API.CrunchBase.PersonQuery
import Data.API.CrunchBase.Person
import Data.API.CrunchBase.FinancialOrganizationQuery
import Data.API.CrunchBase.FinancialOrganization
import Data.API.CrunchBase.ProductQuery
import Data.API.CrunchBase.Product
import Data.API.CrunchBase.ServiceProviderQuery
import Data.API.CrunchBase.ServiceProvider

import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy as B
import Data.Text (Text)
import Data.Aeson (decode)

-- | Search using Text.
getSearchPage :: Text -> IO (Maybe SearchPage)
getSearchPage = fmap (decode . responseBody) . sendRequest . SearchQuery

-- | Get a Company using a CompanyPermalink.
getCompany :: CompanyPermalink -> IO (Maybe Company)
getCompany = fmap (decode . responseBody) . sendRequest . CompanyQuery

-- | Get a Person using a PersonPermalink.
getPerson :: PersonPermalink -> IO (Maybe Person)
getPerson = fmap (decode . responseBody) . sendRequest . PersonQuery

-- | Get a FinancialOrganization using a FinancialOrganiziationPermalink.
getFinancialOrganization :: FinancialOrganizationPermalink
                         -> IO (Maybe FinancialOrganization)
getFinancialOrganization =
  fmap (decode . responseBody) . sendRequest . FinancialOrganizationQuery

-- | Get a Product using a ProductPermalink.
getProduct :: ProductPermalink -> IO (Maybe Product)
getProduct = fmap (decode . responseBody) . sendRequest . ProductQuery

-- | Get a ServiceProvider using a ServiceProviderPermalink.
getServiceProvider :: ServiceProviderPermalink -> IO (Maybe ServiceProvider)
getServiceProvider = fmap (decode . responseBody) . sendRequest . ServiceProviderQuery

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
