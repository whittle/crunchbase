{-# LANGUAGE OverloadedStrings #-}
module Data.API.CrunchBase.Company
       ( Company(..)
       ) where

import Data.API.CrunchBase.Response
import Data.API.CrunchBase.CompanyQuery (CompanyPermalink(..))
import Data.API.CrunchBase.Image
import Data.API.CrunchBase.VideoEmbed

import Data.Aeson
import Data.Text (Text)
import Control.Applicative

data Company = Company { name :: Text
                       , permalink :: CompanyPermalink
                       , crunchbaseUrl :: Text
                       , homepageUrl :: Maybe Text
                       , blogUrl :: Maybe Text
                       , blogFeedUrl :: Maybe Text
                       , twitterUsername :: Maybe Text
                       , categoryCode :: Maybe Text
                       , numberOfEmployees :: Maybe Integer
                       , foundedYear :: Maybe Integer
                       , foundedMonth :: Maybe Integer
                       , foundedDay :: Maybe Integer
                       , deadpooledYear :: Maybe Integer
                       , deadpooledMonth :: Maybe Integer
                       , deadpooledDay :: Maybe Integer
                       , deadpooledUrl :: Maybe Text
                       , tagList :: Maybe Text
                       , aliasList :: Maybe Text
                       , emailAddress :: Maybe Text
                       , phoneNumber :: Maybe Text
                       , description :: Maybe Text
                       , createdAt :: Maybe Text
                       , updatedAt :: Maybe Text
                       , overview :: Maybe Text
                       , image :: Maybe Image
                       , products :: [Object]
                       , relationships :: [Object]
                       , competitions :: [Object]
                       , providerships :: [Object]
                       , totalMoneyRaised :: Maybe Text
                       , fundingRounds :: [Object]
                       , investments :: [Object]
                       , acquisition :: Maybe Object
                       , acquisitions :: [Object]
                       , offices :: [Object]
                       , milestones :: [Object]
                       , ipo :: Maybe Object
                       , videoEmbeds :: [VideoEmbed]
                       , screenshots :: [Object]
                       , externalLinks :: [Object]
                       } deriving (Eq, Show)

instance FromJSON Company where
  parseJSON (Object o) = Company
                         <$> o .: "name"
                         <*> o .: "permalink"
                         <*> o .: "crunchbase_url"
                         <*> o .:- "homepage_url"
                         <*> o .:- "blog_url"
                         <*> o .:- "blog_feed_url"
                         <*> o .:- "twitter_username"
                         <*> o .:- "category_code"
                         <*> o .:? "number_of_employees"
                         <*> o .:? "founded_year"
                         <*> o .:? "founded_month"
                         <*> o .:? "founded_day"
                         <*> o .:? "deadpooled_year"
                         <*> o .:? "deadpooled_month"
                         <*> o .:? "deadpooled_day"
                         <*> o .:- "deadpooled_url"
                         <*> o .:- "tag_list"
                         <*> o .:- "alias_list"
                         <*> o .:- "email_address"
                         <*> o .:- "phone_number"
                         <*> o .:- "description"
                         <*> o .:- "created_at"
                         <*> o .:- "updated_at"
                         <*> o .:- "overview"
                         <*> o .:? "image"
                         <*> o .: "products"
                         <*> o .: "relationships"
                         <*> o .: "competitions"
                         <*> o .: "providerships"
                         <*> o .:? "total_money_raised"
                         <*> o .: "funding_rounds"
                         <*> o .: "investments"
                         <*> o .:? "acquisition"
                         <*> o .: "acquisitions"
                         <*> o .: "offices"
                         <*> o .: "milestones"
                         <*> o .:? "ipo"
                         <*> o .: "video_embeds"
                         <*> o .: "screenshots"
                         <*> o .: "external_links"
