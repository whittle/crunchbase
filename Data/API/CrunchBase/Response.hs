-- | Helper functions for decoding JSON.
module Data.API.CrunchBase.Response
       ( (.:-)
       , mkDate
       ) where

import Data.Time.FuzzyDate
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Text as T
import Control.Applicative

(.:-) :: Object -> T.Text -> Parser (Maybe T.Text)
object .:- key = fmap unnull (object .:? key)

unnull :: Maybe T.Text -> Maybe T.Text
unnull Nothing = Nothing
unnull (Just text) | T.null text = Nothing
                   | otherwise = Just text

mkDate :: Object
       -> T.Text -- ^year key
       -> T.Text -- ^month key
       -> T.Text -- ^day key
       -> Parser (Maybe FuzzyDate)
mkDate o year month day = fuzzyDateValid
                          <$> o .:? year
                          <*> o .:? month
                          <*> o .:? day
