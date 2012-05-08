{-# LANGUAGE DeriveGeneric #-}
module Data.API.CrunchBase.PersonResponse
       (
         Person(..)
       ) where

import qualified Data.ByteString.Lazy as B
import Data.Aeson
--import Data.Time.Clock
import GHC.Generics

data Person = Person { first_name :: String
                     , last_name :: String
                     , permalink :: String
                     , crunchbase_url :: String
                     , homepage_url :: Maybe String
                     , birthplace :: Maybe String
                     , twitter_username :: Maybe String
                     , blog_url :: Maybe String
                     , blog_feed_url :: Maybe String
                     , affiliation_name :: String
                     , born_year :: Maybe String
                     , born_month :: Maybe String
                     , born_day :: Maybe String
                     , tag_list :: Maybe String
                     , alias_list :: Maybe String
                     , created_at :: String --UTCTime
                     , updated_at :: String --UTCTime
                     , overview :: Maybe String
                     , image :: Maybe String
                     , degrees :: [Degree]
                     , relationships :: [Relationship]
                     , investments :: [Investment]
                     , milestones :: [Milestone]
                     , video_embeds :: [VideoEmbed]
                     , external_links :: [ExternalLink]
                     , web_presences :: [WebPresence]
                     } deriving (Eq, Show, Generic)

instance FromJSON Person

type Degree = String
type Relationship = Object
type Investment = String
type Milestone = String
type VideoEmbed = String
type ExternalLink = String
type WebPresence = String
