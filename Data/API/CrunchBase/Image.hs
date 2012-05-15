{-# LANGUAGE OverloadedStrings #-}
module Data.API.CrunchBase.Image
       ( Image(..)
       ) where

import Data.Aeson
import Data.Text (Text)
import Data.Attoparsec.Number (Number)
import Control.Applicative
import qualified Data.Vector as V

data Image = Image { attribution :: Text
                   , availableSizes :: [AvailableSize]
                   } deriving (Eq, Show)

instance FromJSON Image where
  parseJSON (Object o) = Image
                         <$> o .: "attribution"
                         <*> o .: "available_sizes"

data AvailableSize = AvailableSize { width :: Number
                                   , height :: Number
                                   , url :: Text
                                   } deriving (Eq, Show)

instance FromJSON AvailableSize where
  parseJSON (Array a) = return $ AvailableSize
                        { width = unpackHead . V.head $ a
                        , height = unpackLast . V.head $ a
                        , url = unpackString . V.last $ a
                        }
    where unpackHead (Array v) = unpackInteger . V.head $ v
          unpackLast (Array v) = unpackInteger . V.last $ v
          unpackInteger (Number n) = n
          unpackString (String t) = t
