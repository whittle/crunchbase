{-# LANGUAGE OverloadedStrings #-}
module Data.API.CrunchBase.SearchResult
       ( SearchResult(..)
       , mkCompany
       ) where

import Data.API.CrunchBase.Response
import Data.API.CrunchBase.CompanyQuery (CompanyPermalink(..))
import Data.API.CrunchBase.PersonQuery (PersonPermalink(..))
import Data.API.CrunchBase.FinancialOrganizationQuery (FinancialOrganizationPermalink(..))
import Data.API.CrunchBase.ProductQuery (ProductPermalink(..))
import Data.API.CrunchBase.ServiceProviderQuery (ServiceProviderPermalink(..))
import Data.API.CrunchBase.Image (Image(..))

import Data.Aeson (Value(..), Object, FromJSON(..), (.:))
import Data.Aeson.Types (Parser)
import Control.Applicative
import Data.Text (Text)

data SearchResult = Company
                    { name :: Text
                    , companyPermalink :: CompanyPermalink
                    , crunchbaseUrl :: Maybe Text
                    , overview :: Maybe Text
                    , image :: Maybe Image }
                  | Person
                    { firstName :: Text
                    , lastName :: Text
                    , personPermalink :: PersonPermalink
                    , crunchbaseUrl :: Maybe Text
                    , overview :: Maybe Text
                    , image :: Maybe Image }
                  | FinancialOrganization
                    { name :: Text
                    , financialOrganizationPermalink :: FinancialOrganizationPermalink
                    , crunchbaseUrl :: Maybe Text
                    , overview :: Maybe Text
                    , image :: Maybe Image }
                  | Product
                    { name :: Text
                    , productPermalink :: ProductPermalink
                    , crunchbaseUrl :: Maybe Text
                    , overview :: Maybe Text
                    , image :: Maybe Image }
                  | ServiceProvider
                    { name :: Text
                    , serviceProviderPermalink :: ServiceProviderPermalink
                    , crunchbaseUrl :: Maybe Text
                    , overview :: Maybe Text
                    , image :: Maybe Image }
                  deriving (Eq, Show)

instance FromJSON SearchResult where
  parseJSON v@(Object o) = o .: "namespace" >>= flip mkSearchResult v

mkSearchResult :: Text -> Value -> Parser SearchResult
mkSearchResult "company" = mkCompany
mkSearchResult "person" = mkPerson
mkSearchResult "financial-organization" = mkFinancialOrganization
mkSearchResult "product" = mkProduct
mkSearchResult "service-provider" = mkServiceProvider

mkCompany :: Value -> Parser SearchResult
mkCompany (Object o) = Company
                       <$> o .: "name"
                       <*> o .: "permalink"
                       <*> o .:- "crunchbase_url"
                       <*> o .:- "overview"
                       <*> o .: "image"

mkPerson :: Value -> Parser SearchResult
mkPerson (Object o) = Person
                      <$> o .: "first_name"
                      <*> o .: "last_name"
                      <*> o .: "permalink"
                      <*> o .:- "crunchbase_url"
                      <*> o .:- "overview"
                      <*> o .: "image"

mkFinancialOrganization :: Value -> Parser SearchResult
mkFinancialOrganization (Object o) = FinancialOrganization
                                     <$> o .: "name"
                                     <*> o .: "permalink"
                                     <*> o .:- "crunchbase_url"
                                     <*> o .:- "overview"
                                     <*> o .: "image"

mkProduct :: Value -> Parser SearchResult
mkProduct (Object o) = Product
                       <$> o .: "name"
                       <*> o .: "permalink"
                       <*> o .:- "crunchbase_url"
                       <*> o .:- "overview"
                       <*> o .: "image"

mkServiceProvider :: Value -> Parser SearchResult
mkServiceProvider (Object o) = ServiceProvider
                               <$> o .: "name"
                               <*> o .: "permalink"
                               <*> o .:- "crunchbase_url"
                               <*> o .:- "overview"
                               <*> o .: "image"
