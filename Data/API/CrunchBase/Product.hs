{-# LANGUAGE OverloadedStrings #-}
module Data.API.CrunchBase.Product
       ( Product(..)
       ) where

import Data.API.CrunchBase.Response
import Data.API.CrunchBase.ProductQuery (ProductPermalink (..))

import Data.Aeson
import Data.Text (Text)
import Control.Applicative

data Product = Product { name :: Text
                       , permalink :: ProductPermalink
                       , crunchbaseUrl :: Text
                       , homepageUrl :: Maybe Text
                       , blogUrl :: Maybe Text
                       , blogFeedUrl :: Maybe Text
                       , twitterUsername :: Maybe Text
                       , stageCode :: Maybe Text
                       , deadpooledUrl :: Maybe Text
                       , inviteShareUrl :: Maybe Text
                       , tagList :: Maybe Text
                       , aliasList :: Maybe Text
                       , deadpooledYear :: Maybe Integer
                       , deadpooledMonth :: Maybe Integer
                       , deadpooledDay :: Maybe Integer
                       , launchedYear :: Maybe Integer
                       , launchedMonth :: Maybe Integer
                       , launchedDay :: Maybe Integer
                       , createdAt :: Maybe Text
                       , updatedAt :: Maybe Text
                       , overview :: Maybe Text
                       , image :: Maybe Object
                       , company :: Maybe Object
                       , milestones :: [Object]
                       , videoEmbeds :: [Object]
                       , externalLinks :: [Object]
                       } deriving (Eq, Show)

instance FromJSON Product where
  parseJSON (Object o) = Product
                         <$> o .: "name"
                         <*> o .: "permalink"
                         <*> o .: "crunchbase_url"
                         <*> o .:- "homepage_url"
                         <*> o .:- "blog_url"
                         <*> o .:- "blog_feed_url"
                         <*> o .:- "twitter_username"
                         <*> o .:- "stage_code"
                         <*> o .:- "deadpooled_url"
                         <*> o .:- "invite_share_url"
                         <*> o .:- "tag_list"
                         <*> o .:- "alias_list"
                         <*> o .:? "deadpooled_year"
                         <*> o .:? "deadpooled_month"
                         <*> o .:? "deadpooled_day"
                         <*> o .:? "launched_year"
                         <*> o .:? "launched_month"
                         <*> o .:? "launched_day"
                         <*> o .:- "created_at"
                         <*> o .:- "updated_at"
                         <*> o .:- "overview"
                         <*> o .:? "image"
                         <*> o .:? "company"
                         <*> o .: "milestones"
                         <*> o .: "video_embeds"
                         <*> o .: "external_links"

instance FromJSON ProductPermalink where
  parseJSON (String s) = return . ProductPermalink $ s
