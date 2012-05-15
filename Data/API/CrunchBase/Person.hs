{-# LANGUAGE OverloadedStrings #-}
module Data.API.CrunchBase.Person
       ( Person(..)
       , Degree(..)
       , Relationship(..)
       , WebPresence(..)
       ) where

import Data.API.CrunchBase.Response
import Data.API.CrunchBase.PersonQuery (PersonPermalink(..))
import qualified Data.API.CrunchBase.SearchResult as S
import Data.API.CrunchBase.Image

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
                     , image :: Maybe Image
                     , degrees :: [Degree]
                     , relationships :: [Relationship]
                     , investments :: [Object]
                     , milestones :: [Object]
                     , videoEmbeds :: [Object]
                     , externalLinks :: [Object]
                     , webPresences :: [WebPresence]
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

data Degree = Degree { degreeType :: Maybe Text
                     , subject :: Maybe Text
                     , institution :: Maybe Text
                     , graduatedYear :: Maybe Integer
                     , graduatedMonth :: Maybe Integer
                     , graduatedDay :: Maybe Integer
                     } deriving (Eq, Show)

instance FromJSON Degree where
  parseJSON (Object o) = Degree
                         <$> o .:- "degree_type"
                         <*> o .:- "subject"
                         <*> o .:- "institution"
                         <*> o .:? "graduated_year"
                         <*> o .:? "graduated_month"
                         <*> o .:? "graduated_day"

data Relationship = Relationship { isPast :: Bool
                                  , title :: Text
                                  , firm :: S.SearchResult
                                  } deriving (Eq, Show)

instance FromJSON Relationship where
  parseJSON (Object o) = Relationship
                         <$> o .: "is_past"
                         <*> o .: "title"
                         <*> ((o .: "firm") >>= S.mkCompany)

data WebPresence = WebPresence { externalUrl :: Text
                               , siteName :: Text
                               } deriving (Eq, Show)

instance FromJSON WebPresence where
  parseJSON (Object o) = WebPresence
                         <$> o .: "external_url"
                         <*> o .: "title"
