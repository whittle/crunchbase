{-# LANGUAGE OverloadedStrings #-}
module Data.API.CrunchBase.VideoEmbed
       ( VideoEmbed(..)
       ) where

import Data.API.CrunchBase.Response ((.:-))

import Data.Aeson
import Data.Text (Text)
import Control.Applicative

data VideoEmbed = VideoEmbed { embedCode :: Maybe Text
                             , description :: Maybe Text
                             } deriving (Eq, Show)

instance FromJSON VideoEmbed where
  parseJSON (Object o) = VideoEmbed
                         <$> o .:- "embed_code"
                         <*> o .:- "description"
