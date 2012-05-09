{-# LANGUAGE OverloadedStrings #-}
module Data.API.CrunchBase.CompanyResponse
       (
         Company(..)
       ) where

import Data.API.CrunchBase.CompanyQuery (CompanyPermalink(..))

import Data.Aeson
import Data.Text (Text)
import Control.Applicative

data Company = Company { name :: Text
                       , permalink :: CompanyPermalink
                       , crunchbase_url :: Text
                       , homepage_url :: Maybe Text
                       , blog_url :: Maybe Text
                       , blog_feed_url :: Maybe Text
                       , twitter_username :: Maybe Text
                       , category_code :: Maybe Text
                       , number_of_employees :: Maybe Integer
                       , founded_year :: Maybe Integer
                       , founded_month :: Maybe Integer
                       , founded_day :: Maybe Integer
                       , deadpooled_year :: Maybe Integer
                       , deadpooled_month :: Maybe Integer
                       , deadpooled_day :: Maybe Integer
                       , deadpooled_url :: Maybe Integer
                       , tag_list :: Maybe Text
                       , alias_list :: Maybe Text
                       , email_address :: Maybe Text
                       , phone_number :: Maybe Text
                       , description :: Maybe Text
                       , created_at :: Maybe Text
                       , updated_at :: Maybe Text
                       , overview :: Maybe Text
                       , image :: Maybe Object
                       , products :: [Object]
                       , relationships :: [Object]
                       , competitions :: [Object]
                       , providerships :: [Object]
                       , total_money_raised :: Maybe Text
                       , funding_rounds :: [Object]
                       , investments :: [Object]
                       , acquisition :: Maybe Object
                       , acquisitions :: [Object]
                       , offices :: [Object]
                       , milestones :: [Object]
                       , ipo :: Maybe Object
                       , video_embeds :: [Object]
                       , screenshots :: [Object]
                       , external_links :: [Object]
                       } deriving (Eq, Show)

instance FromJSON Company where
  parseJSON (Object o) = Company
                       <$> o .: "name"
                       <*> o .: "permalink"
                       <*> o .: "crunchbase_url"
                       <*> o .:? "homepage_url"
                       <*> o .:? "blog_url"
                       <*> o .:? "blog_feed_url"
                       <*> o .:? "twitter_username"
                       <*> o .:? "category_code"
                       <*> o .:? "number_of_employees"
                       <*> o .:? "founded_year"
                       <*> o .:? "founded_month"
                       <*> o .:? "founded_day"
                       <*> o .:? "deadpooled_year"
                       <*> o .:? "deadpooled_month"
                       <*> o .:? "deadpooled_day"
                       <*> o .:? "deadpooled_url"
                       <*> o .:? "tag_list"
                       <*> o .:? "alias_list"
                       <*> o .:? "email_address"
                       <*> o .:? "phone_number"
                       <*> o .:? "description"
                       <*> o .:? "created_at"
                       <*> o .:? "updated_at"
                       <*> o .:? "overview"
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

instance FromJSON CompanyPermalink where
  parseJSON (String s) = return . CompanyPermalink $ s
