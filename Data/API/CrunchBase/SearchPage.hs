{-# LANGUAGE OverloadedStrings #-}
module Data.API.CrunchBase.SearchPage
       ( SearchPage(..)
       ) where

import Data.API.CrunchBase.Response
import Data.API.CrunchBase.SearchResult

import Data.Aeson
import Data.Text (Text)
import Control.Applicative

data SearchPage = SearchPage { total :: Integer
                             , page :: Integer
                             , crunchbaseUrl :: Text
                             , results :: [SearchResult]
                             } deriving (Eq, Show)

instance FromJSON SearchPage where
  parseJSON (Object o) = SearchPage
                         <$> o .: "total"
                         <*> o .: "page"
                         <*> o .: "crunchbase_url"
                         <*> o .: "results"
