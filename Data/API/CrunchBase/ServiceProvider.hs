{-# LANGUAGE OverloadedStrings #-}
module Data.API.CrunchBase.ServiceProvider
       ( ServiceProvider(..)
       ) where

import Data.API.CrunchBase.Response
import Data.API.CrunchBase.ServiceProviderQuery (ServiceProviderPermalink(..))
import Data.API.CrunchBase.Image

import Data.Aeson
import Data.Text (Text)
import Control.Applicative

data ServiceProvider = ServiceProvider
                       { name :: Text
                       , permalink :: ServiceProviderPermalink
                       , crunchbaseUrl :: Text
                       , homepageUrl :: Maybe Text
                       , phoneNumber :: Maybe Text
                       , emailAddress :: Maybe Text
                       , tagList :: Maybe Text
                       , aliasList :: Maybe Text
                       , createdAt :: Maybe Text
                       , updatedAt :: Maybe Text
                       , overview :: Maybe Text
                       , image :: Maybe Image
                       , offices :: [Object]
                       , providerships :: [Object]
                       , externalLinks :: [Object]
                       } deriving (Eq, Show)

instance FromJSON ServiceProvider where
  parseJSON (Object o) = ServiceProvider
                         <$> o .: "name"
                         <*> o .: "permalink"
                         <*> o .: "crunchbase_url"
                         <*> o .:- "homepage_url"
                         <*> o .:- "phone_number"
                         <*> o .:- "email_address"
                         <*> o .:- "tag_list"
                         <*> o .:- "alias_list"
                         <*> o .:- "created_at"
                         <*> o .:- "updated_at"
                         <*> o .:- "overview"
                         <*> o .:? "image"
                         <*> o .: "offices"
                         <*> o .: "providerships"
                         <*> o .: "external_links"
