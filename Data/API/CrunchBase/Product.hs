{-# LANGUAGE OverloadedStrings #-}
module Data.API.CrunchBase.Product
       ( Product(..)
       ) where

import Data.API.CrunchBase.Response
import Data.API.CrunchBase.ProductQuery (ProductPermalink (..))
import Data.API.CrunchBase.Image
import Data.API.CrunchBase.VideoEmbed
import Data.API.CrunchBase.ExternalLink
import Data.Time.FuzzyDate

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
                       , deadpooledDate :: Maybe FuzzyDate
                       , launchedDate :: Maybe FuzzyDate
                       , createdAt :: Maybe Text
                       , updatedAt :: Maybe Text
                       , overview :: Maybe Text
                       , image :: Maybe Image
                       , company :: Maybe Object
                       , milestones :: [Object]
                       , videoEmbeds :: [VideoEmbed]
                       , externalLinks :: [ExternalLink]
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
                         <*> mkDate o "deadpooled_year" "deadpooled_month" "deadpooled_day"
                         <*> mkDate o "launched_year" "launched_month" "launched_day"
                         <*> o .:- "created_at"
                         <*> o .:- "updated_at"
                         <*> o .:- "overview"
                         <*> o .:? "image"
                         <*> o .:? "company"
                         <*> o .: "milestones"
                         <*> o .: "video_embeds"
                         <*> o .: "external_links"
