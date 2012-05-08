{-# LANGUAGE OverloadedStrings #-}
module Data.API.CrunchBase.ProductQuery
       ( ProductQuery(..)
       , ProductPermalink(..)
       ) where

import Data.API.CrunchBase.Query
import qualified Data.Text as T

newtype ProductPermalink = ProductPermalink T.Text deriving (Eq, Show)

newtype ProductQuery = ProductQuery ProductPermalink deriving (Eq, Show)

instance Query ProductQuery where
  toPathSegments (ProductQuery (ProductPermalink permalink)) =
    [ "v"
    , "1"
    , "product"
    , permalink `T.append` ".js"
    ]

  toQueryItems _ = []
