{-# LANGUAGE DeriveGeneric #-}
module Data.API.CrunchBase.PersonResponse
       (
         Person(..)
       ) where

import Data.Aeson
import GHC.Generics
import qualified Data.Text as T
--import Data.Time.Clock

data Person = Person { first_name :: T.Text
                     , last_name :: T.Text
                     , permalink :: T.Text
                     , crunchbase_url :: T.Text
                     , homepage_url :: Maybe T.Text
                     , birthplace :: Maybe T.Text
                     , twitter_username :: Maybe T.Text
                     , blog_url :: Maybe T.Text
                     , blog_feed_url :: Maybe T.Text
                     , affiliation_name :: T.Text
                     , born_year :: Maybe T.Text
                     , born_month :: Maybe T.Text
                     , born_day :: Maybe T.Text
                     , tag_list :: Maybe T.Text
                     , alias_list :: Maybe T.Text
                     , created_at :: T.Text --UTCTime
                     , updated_at :: T.Text --UTCTime
                     , overview :: Maybe T.Text
                     , image :: Maybe T.Text
                     , degrees :: [Degree]
                     , relationships :: [Relationship]
                     , investments :: [Investment]
                     , milestones :: [Milestone]
                     , video_embeds :: [VideoEmbed]
                     , external_links :: [ExternalLink]
                     , web_presences :: [WebPresence]
                     } deriving (Eq, Show, Generic)

instance FromJSON Person

type Degree = T.Text
type Relationship = Object
type Investment = T.Text
type Milestone = T.Text
type VideoEmbed = T.Text
type ExternalLink = T.Text
type WebPresence = T.Text
