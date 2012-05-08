{-# LANGUAGE OverloadedStrings #-}
module Data.API.CrunchBase.FinancialOrganizationQuery
       ( FinancialOrganizationQuery(..)
       , FinancialOrganizationPermalink(..)
       ) where

import Data.API.CrunchBase.Query
import qualified Data.Text as T

newtype FinancialOrganizationPermalink =
  FinancialOrganizationPermalink T.Text
  deriving (Eq, Show)

newtype FinancialOrganizationQuery =
  FinancialOrganizationQuery FinancialOrganizationPermalink
  deriving (Eq, Show)

instance Query FinancialOrganizationQuery where
  toPathSegments (FinancialOrganizationQuery
                  (FinancialOrganizationPermalink permalink)) =
    [ "v"
    , "1"
    , "financial-organization"
    , permalink `T.append` ".js"
    ]

  toQueryItems _ = []
