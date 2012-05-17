{-# LANGUAGE OverloadedStrings #-}
module Data.API.CrunchBase.FinancialOrganization
       ( FinancialOrganization(..)
       ) where

import Data.API.CrunchBase.Response
import Data.API.CrunchBase.FinancialOrganizationQuery (FinancialOrganizationPermalink(..))
import Data.API.CrunchBase.Image

import Data.Aeson
import Data.Text (Text)
import Control.Applicative

data FinancialOrganization = FinancialOrganization
                             { name :: Text
                             , permalink :: FinancialOrganizationPermalink
                             , crunchbaseUrl :: Text
                             , homepageUrl :: Maybe Text
                             , blogUrl :: Maybe Text
                             , blogFeedUrl :: Maybe Text
                             , twitterUsername :: Maybe Text
                             , phoneNumber :: Maybe Text
                             , description :: Maybe Text
                             , emailAddress :: Maybe Text
                             , numberOfEmployees :: Maybe Integer
                             , foundedYear :: Maybe Integer
                             , foundedMonth :: Maybe Integer
                             , foundedDay :: Maybe Integer
                             , tagList :: Maybe Text
                             , aliasList :: Maybe Text
                             , createdAt :: Maybe Text
                             , updatedAt :: Maybe Text
                             , overview :: Maybe Text
                             , image :: Maybe Image
                             , offices :: [Object]
                             , relationships :: [Object]
                             , investments :: [Object]
                             , milestones :: [Object]
                             , providerships :: [Object]
                             , funds :: [Object]
                             , videoEmbeds :: [Object]
                             , externalLinks :: [Object]
                             } deriving (Eq, Show)

instance FromJSON FinancialOrganization where
  parseJSON (Object o) = FinancialOrganization
                         <$> o .: "name"
                         <*> o .: "permalink"
                         <*> o .: "crunchbase_url"
                         <*> o .:- "homepage_url"
                         <*> o .:- "blog_url"
                         <*> o .:- "blog_feed_url"
                         <*> o .:- "twitter_username"
                         <*> o .:- "phone_number"
                         <*> o .:- "description"
                         <*> o .:- "email_address"
                         <*> o .:? "number_of_employees"
                         <*> o .:? "founded_year"
                         <*> o .:? "founded_month"
                         <*> o .:? "founded_day"
                         <*> o .:- "tag_list"
                         <*> o .:- "alias_list"
                         <*> o .:- "created_at"
                         <*> o .:- "updated_at"
                         <*> o .:- "overview"
                         <*> o .:? "image"
                         <*> o .: "offices"
                         <*> o .: "relationships"
                         <*> o .: "investments"
                         <*> o .: "milestones"
                         <*> o .: "providerships"
                         <*> o .: "funds"
                         <*> o .: "video_embeds"
                         <*> o .: "external_links"
