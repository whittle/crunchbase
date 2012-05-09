-- | Helper functions for decoding JSON.
module Data.API.CrunchBase.Response ((.:-)) where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Text as T

(.:-) :: Object -> T.Text -> Parser (Maybe T.Text)
object .:- key = fmap unnull (object .:? key)

unnull :: Maybe T.Text -> Maybe T.Text
unnull Nothing = Nothing
unnull (Just text) | T.null text = Nothing
                   | otherwise = Just text
