{-# LANGUAGE OverloadedStrings #-}
module Data.API.CrunchBase.ExternalLink
       ( ExternalLink(..)
       ) where

import Data.API.CrunchBase.Response ((.:-))

import Data.Aeson
import Data.Text (Text)
import Control.Applicative

data ExternalLink = ExternalLink { externalUrl :: Text
                                 , title :: Maybe Text
                                 } deriving (Eq, Show)

instance FromJSON ExternalLink where
  parseJSON (Object o) = ExternalLink
                         <$> o .: "external_url"
                         <*> o .:- "title"
