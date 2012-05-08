{-# LANGUAGE OverloadedStrings #-}
module Data.API.CrunchBase.SearchResult where

import Data.Aeson (Value (..), Object, FromJSON (..))
import Data.Aeson ((.:))
import Data.Aeson.Types (Parser)
import Control.Applicative
import Data.Text (Text)

data Result = Company
              { name :: Text
              , permalink :: Text
              , crunchbaseUrl :: Text
              , overview :: Maybe Text
              , image :: Maybe Object }
            | Person
              { firstName :: Text
              , lastName :: Text
              , permalink :: Text
              , crunchbaseUrl :: Text
              , overview :: Maybe Text
              , image :: Maybe Object }
            | FinancialOrganization
              { name :: Text
              , permalink :: Text
              , crunchbaseUrl :: Text
              , overview :: Maybe Text
              , image :: Maybe Object }
            | Product
              { name :: Text
              , permalink :: Text
              , crunchbaseUrl :: Text
              , overview :: Maybe Text
              , image :: Maybe Object }
            | ServiceProvider
              { name :: Text
              , permalink :: Text
              , crunchbaseUrl :: Text
              , overview :: Maybe Text
              , image :: Maybe Object }
            deriving (Eq, Show)

instance FromJSON Result where
  parseJSON v@(Object o) = o .: "namespace" >>= flip mkResult v

mkResult :: Text -> Value -> Parser Result
mkResult "company" = mkCompany
mkResult "person" = mkPerson
mkResult "financial-organization" = mkFinancialOrganization
mkResult "product" = mkProduct
mkResult "service-provider" = mkServiceProvider

mkCompany :: Value -> Parser Result
mkCompany (Object o) = Company
                       <$> o .: "name"
                       <*> o .: "permalink"
                       <*> o .: "crunchbase_url"
                       <*> o .: "overview"
                       <*> o .: "image"

mkPerson :: Value -> Parser Result
mkPerson (Object o) = Person
                      <$> o .: "first_name"
                      <*> o .: "last_name"
                      <*> o .: "permalink"
                      <*> o .: "crunchbase_url"
                      <*> o .: "overview"
                      <*> o .: "image"

mkFinancialOrganization :: Value -> Parser Result
mkFinancialOrganization (Object o) = FinancialOrganization
                                     <$> o .: "name"
                                     <*> o .: "permalink"
                                     <*> o .: "crunchbase_url"
                                     <*> o .: "overview"
                                     <*> o .: "image"

mkProduct :: Value -> Parser Result
mkProduct (Object o) = Product
                       <$> o .: "name"
                       <*> o .: "permalink"
                       <*> o .: "crunchbase_url"
                       <*> o .: "overview"
                       <*> o .: "image"

mkServiceProvider :: Value -> Parser Result
mkServiceProvider (Object o) = ServiceProvider
                               <$> o .: "name"
                               <*> o .: "permalink"
                               <*> o .: "crunchbase_url"
                               <*> o .: "overview"
                               <*> o .: "image"
