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
import Data.API.CrunchBase.VideoEmbed
import Data.API.CrunchBase.ExternalLink
import Data.Time.FuzzyDate

import Data.Aeson
import Data.Aeson.Types (Parser)
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
                     , bornDate :: Maybe FuzzyDate
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
                     , videoEmbeds :: [VideoEmbed]
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
                         <*> mkDate o "born_year" "born_month" "born_day"
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
                     , graduatedDate :: Maybe FuzzyDate
                     } deriving (Eq, Show)

instance FromJSON Degree where
  parseJSON (Object o) = Degree
                         <$> o .:- "degree_type"
                         <*> o .:- "subject"
                         <*> o .:- "institution"
                         <*> mkDate o "graduated_year" "graduated_month" "graduated_day"

data Relationship = Relationship { isPast :: Maybe Bool
                                 , title :: Maybe Text
                                 , firm :: S.SearchResult
                                 } deriving (Eq, Show)

instance FromJSON Relationship where
  parseJSON (Object o) = Relationship
                         <$> o .:? "is_past"
                         <*> o .:- "title"
                         <*> ((o .: "firm") >>= mkFirm)

mkFirm :: Value -> Parser S.SearchResult
mkFirm v@(Object o) = o .: "type_of_entity" >>= flip mkFirm' v

mkFirm' :: Text -> Value -> Parser S.SearchResult
mkFirm' "company" = S.mkCompany
mkFirm' "financial_org" = S.mkFinancialOrganization

data Investment = Investment { fundingRound :: FundingRound
                             } deriving (Eq, Show)

instance FromJSON Investment where
  parseJSON (Object o) = Investment <$> o .: "funding_round"

data Milestone = Milestone { description :: Text
                           , stonedDate :: Maybe FuzzyDate
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
                         <*> mkDate o "stoned_year" "stoned_month" "stoned_day"
                         <*> o .:- "source_url"
                         <*> o .:- "source_text"
                         <*> o .:- "source_description"
                         <*> o .:? "stoned_value"
                         <*> o .:? "stoned_value_type"
                         <*> o .:? "stoned_acquirer"
