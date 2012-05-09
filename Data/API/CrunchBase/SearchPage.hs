{-# LANGUAGE DeriveGeneric #-}
module Data.API.CrunchBase.SearchResponse where

import Data.API.CrunchBase.SearchResult

import qualified Data.Aeson as A
import Data.Attoparsec.Number
import GHC.Generics

data Page = Page { total :: Number
                 , page :: Number
                 , crunchbase_url :: String
                 , results :: [SearchResult]
                 } deriving (Eq, Show, Generic)

instance A.FromJSON Page
