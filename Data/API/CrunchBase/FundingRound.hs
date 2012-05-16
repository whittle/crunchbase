{-# LANGUAGE OverloadedStrings #-}
module Data.API.CrunchBase.FundingRound
       ( FundingRound(..)
       ) where

import Data.API.CrunchBase.Response ((.:-))
import qualified Data.API.CrunchBase.SearchResult as S

import Data.Aeson
import Data.Text (Text)
import Data.Attoparsec.Number (Number)
import Control.Applicative

data FundingRound = FundingRound { roundCode :: Maybe Text
                                 , sourceUrl :: Maybe Text
                                 , sourceDescription :: Maybe Text
                                 , raisedAmount :: Maybe Integer
                                 , raisedCurrencyCode :: Maybe Text
                                 , fundedYear :: Maybe Integer
                                 , fundedMonth :: Maybe Integer
                                 , fundedDay :: Maybe Integer
                                 , company :: S.SearchResult
                                 } deriving (Eq, Show)

instance FromJSON FundingRound where
  parseJSON (Object o) = FundingRound
                         <$> o .:- "round_code"
                         <*> o .:- "source_url"
                         <*> o .:- "source_description"
                         <*> o .:? "raised_amount"
                         <*> o .:- "raised_currency_code"
                         <*> o .:? "funded_year"
                         <*> o .:? "funded_month"
                         <*> o .:? "funded_day"
                         <*> ((o .: "company") >>= S.mkCompany)
