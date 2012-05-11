{-# LANGUAGE OverloadedStrings #-}
-- | Querying for a financial organization takes a
--   FinancialOrganizationPermalink, which can be obtained from a
--   search.
module Data.API.CrunchBase.FinancialOrganizationQuery
       ( FinancialOrganizationQuery(..)
       , FinancialOrganizationPermalink(..)
       ) where

import Data.API.CrunchBase.Query
import qualified Data.Text as T
import Data.Aeson (FromJSON(..), Value(..))

newtype FinancialOrganizationQuery =
  FinancialOrganizationQuery FinancialOrganizationPermalink
  deriving (Eq, Show, Read)

instance Query FinancialOrganizationQuery where
  toPathSegments (FinancialOrganizationQuery
                  (FinancialOrganizationPermalink permalink)) =
    ["v", "1", "financial-organization", permalink `T.append` ".js"]

  toQueryItems _ = []

newtype FinancialOrganizationPermalink =
  FinancialOrganizationPermalink T.Text
  deriving (Eq, Show, Read)

instance FromJSON FinancialOrganizationPermalink where
  parseJSON (String s) = return . FinancialOrganizationPermalink $ s
