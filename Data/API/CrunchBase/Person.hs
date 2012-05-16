{-# LANGUAGE OverloadedStrings #-}
module Data.API.CrunchBase.Person
       ( Person(..)
       , Degree(..)
       , Relationship(..)
       , Milestone(..)
       , Investment(..)
       ) where

import Data.API.CrunchBase.Response
import Data.API.CrunchBase.PersonQuery (PersonPermalink(..))
import qualified Data.API.CrunchBase.SearchResult as S
import Data.API.CrunchBase.Image
import Data.API.CrunchBase.FundingRound
import Data.API.CrunchBase.ExternalLink

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
                     , investments :: [Investment]
                     , milestones :: [Milestone]
                     , videoEmbeds :: [Object]
                     , externalLinks :: [ExternalLink]
                     , webPresences :: [ExternalLink]
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

data Relationship = Relationship { isPast :: Maybe Bool
                                 , title :: Maybe Text
                                 , firm :: S.SearchResult
                                 } deriving (Eq, Show)

instance FromJSON Relationship where
  parseJSON (Object o) = Relationship
                         <$> o .:? "is_past"
                         <*> o .:- "title"
                         <*> ((o .: "firm") >>= S.mkCompany)

data Investment = Investment { fundingRound :: FundingRound
                             } deriving (Eq, Show)

instance FromJSON Investment where
  parseJSON (Object o) = Investment <$> o .: "funding_round"

data Milestone = Milestone { description :: Text
                           , stonedYear :: Maybe Integer
                           , stonedMonth :: Maybe Integer
                           , stonedDay :: Maybe Integer
                           , sourceUrl :: Maybe Text
                           , sourceText :: Maybe Text
                           , sourceDescription :: Maybe Text
                           , stonedValue :: Maybe Value
                           , stonedValueType :: Maybe Value
                           , stonedAcquirer :: Maybe Value
                           } deriving (Eq, Show)

instance FromJSON Milestone where
  parseJSON (Object o) = Milestone
                         <$> o .: "description"
                         <*> o .:? "stoned_year"
                         <*> o .:? "stoned_month"
                         <*> o .:? "stoned_day"
                         <*> o .:- "source_url"
                         <*> o .:- "source_text"
                         <*> o .:- "source_description"
                         <*> o .:? "stoned_value"
                         <*> o .:? "stoned_value_type"
                         <*> o .:? "stoned_acquirer"
