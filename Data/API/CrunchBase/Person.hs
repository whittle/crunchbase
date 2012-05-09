{-# LANGUAGE OverloadedStrings #-}
module Data.API.CrunchBase.Person
       ( Person(..)
       ) where

import Data.API.CrunchBase.Response
import Data.API.CrunchBase.PersonQuery (PersonPermalink(..))

import Data.Aeson
import Data.Text (Text)
import Control.Applicative

data Person = Person { firstName :: Text
                     , lastName :: Text
                     , permalink :: PersonPermalink
                     , crunchbaseUrl :: Text
                     , homepageUrl :: Maybe Text
                     , birthplace :: Maybe Text
                     , twitterUsername :: Maybe Text
                     , blogUrl :: Maybe Text
                     , blogFeedUrl :: Maybe Text
                     , affiliationName :: Maybe Text
                     , bornYear :: Maybe Integer
                     , bornMonth :: Maybe Integer
                     , bornDay :: Maybe Integer
                     , tagList :: Maybe Text
                     , aliasList :: Maybe Text
                     , createdAt :: Maybe Text
                     , updatedAt :: Maybe Text
                     , overview :: Maybe Text
                     , image :: Maybe Object
                     , degrees :: [Object]
                     , relationships :: [Object]
                     , investments :: [Object]
                     , milestones :: [Object]
                     , videoEmbeds :: [Object]
                     , externalLinks :: [Object]
                     , webPresences :: [Object]
                     } deriving (Eq, Show)

instance FromJSON Person where
  parseJSON (Object o) = Person
                         <$> o .: "first_name"
                         <*> o .: "last_name"
                         <*> o .: "permalink"
                         <*> o .: "crunchbase_url"
                         <*> o .:- "homepage_url"
                         <*> o .:- "birthplace"
                         <*> o .:- "twitter_username"
                         <*> o .:- "blog_url"
                         <*> o .:- "blog_feed_url"
                         <*> o .:- "affiliation_name"
                         <*> o .:? "born_year"
                         <*> o .:? "born_month"
                         <*> o .:? "born_day"
                         <*> o .:- "tag_list"
                         <*> o .:- "alias_list"
                         <*> o .:- "created_at"
                         <*> o .:- "updated_at"
                         <*> o .:- "overview"
                         <*> o .:? "image"
                         <*> o .: "degrees"
                         <*> o .: "relationships"
                         <*> o .: "investments"
                         <*> o .: "milestones"
                         <*> o .: "video_embeds"
                         <*> o .: "external_links"
                         <*> o .: "web_presences"

instance FromJSON PersonPermalink where
  parseJSON (String s) = return . PersonPermalink $ s