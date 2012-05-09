{-# LANGUAGE OverloadedStrings #-}
module Data.API.CrunchBase.SearchResult
       ( SearchResult(..)
       ) where

import Data.API.CrunchBase.Response
import Data.API.CrunchBase.CompanyQuery (CompanyPermalink(..))
import Data.API.CrunchBase.PersonQuery (PersonPermalink(..))
import Data.API.CrunchBase.FinancialOrganizationQuery (FinancialOrganizationPermalink(..))
import Data.API.CrunchBase.ProductQuery (ProductPermalink(..))
import Data.API.CrunchBase.ServiceProviderQuery (ServiceProviderPermalink(..))

import Data.Aeson (Value(..), Object, FromJSON(..), (.:))
import Data.Aeson.Types (Parser)
import Control.Applicative
import Data.Text (Text)

data SearchResult = Company
                    { name :: Text
                    , companyPermalink :: CompanyPermalink
                    , crunchbaseUrl :: Text
                    , overview :: Maybe Text
                    , image :: Maybe Object }
                  | Person
                    { firstName :: Text
                    , lastName :: Text
                    , personPermalink :: PersonPermalink
                    , crunchbaseUrl :: Text
                    , overview :: Maybe Text
                    , image :: Maybe Object }
                  | FinancialOrganization
                    { name :: Text
                    , financialOrganizationPermalink :: FinancialOrganizationPermalink
                    , crunchbaseUrl :: Text
                    , overview :: Maybe Text
                    , image :: Maybe Object }
                  | Product
                    { name :: Text
                    , productPermalink :: ProductPermalink
                    , crunchbaseUrl :: Text
                    , overview :: Maybe Text
                    , image :: Maybe Object }
                  | ServiceProvider
                    { name :: Text
                    , serviceProviderPermalink :: ServiceProviderPermalink
                    , crunchbaseUrl :: Text
                    , overview :: Maybe Text
                    , image :: Maybe Object }
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
                       <*> o .: "crunchbase_url"
                       <*> o .:- "overview"
                       <*> o .: "image"

mkPerson :: Value -> Parser SearchResult
mkPerson (Object o) = Person
                      <$> o .: "first_name"
                      <*> o .: "last_name"
                      <*> o .: "permalink"
                      <*> o .: "crunchbase_url"
                      <*> o .:- "overview"
                      <*> o .: "image"

mkFinancialOrganization :: Value -> Parser SearchResult
mkFinancialOrganization (Object o) = FinancialOrganization
                                     <$> o .: "name"
                                     <*> o .: "permalink"
                                     <*> o .: "crunchbase_url"
                                     <*> o .:- "overview"
                                     <*> o .: "image"

mkProduct :: Value -> Parser SearchResult
mkProduct (Object o) = Product
                       <$> o .: "name"
                       <*> o .: "permalink"
                       <*> o .: "crunchbase_url"
                       <*> o .:- "overview"
                       <*> o .: "image"

mkServiceProvider :: Value -> Parser SearchResult
mkServiceProvider (Object o) = ServiceProvider
                               <$> o .: "name"
                               <*> o .: "permalink"
                               <*> o .: "crunchbase_url"
                               <*> o .:- "overview"
                               <*> o .: "image"
