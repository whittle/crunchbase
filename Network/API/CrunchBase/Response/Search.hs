{-# LANGUAGE DeriveGeneric #-}

module Network.API.CrunchBase.Response.Search where

import Network.API.CrunchBase.Response.Search.Result

import qualified Data.Aeson as A
import Data.Attoparsec.Number
import GHC.Generics

data Page = Page { total :: Number
                 , page :: Number
                 , crunchbase_url :: String
                 , results :: [Result]
                 } deriving (Eq, Show, Generic)

instance A.FromJSON Page
