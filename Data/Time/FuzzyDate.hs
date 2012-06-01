-- |FuzzyDate is useful for occasions when it is unknown to what
--  precision dates will be specified. It can handle cases where:
--  * only the year is known
--  * both the month and year are known
--  * the day, month and year are all known
module Data.Time.FuzzyDate
       ( FuzzyDate
       , fuzzyDateValid
       , HowFuzzy(..)
       , howFuzzy
       ) where

import Data.Ranged
import Data.Time.Calendar

instance DiscreteOrdered Day where
  adjacent = enumAdjacent
  adjacentBelow = Just . pred

type FuzzyDate = Range Day

fuzzyDateValid :: Maybe Integer -- ^year
               -> Maybe Int     -- ^month
               -> Maybe Int     -- ^day
               -> Maybe FuzzyDate
fuzzyDateValid (Just y) (Just m) (Just d) = Just $ unrange $ toDay y m d
  where unrange a = Range (BoundaryBelow a) (BoundaryAbove a)
fuzzyDateValid (Just y) (Just m) _ = Just $ (toDay y m 1) ... (toDay y (m + 1) 1)
fuzzyDateValid (Just y) _ _ = Just $ (toDay y 1 1) ... (toDay (y + 1) 1 1)
fuzzyDateValid _ _ _ = Nothing

-- TODO: use fromGregorianValid instead
-- #hide
toDay = fromGregorian
-- #hide
a ... b = Range (BoundaryBelow a) (BoundaryAbove $ pred b)


data HowFuzzy = ToTheDay | ToTheMonth | ToTheYear deriving (Eq, Show)

howFuzzy :: FuzzyDate -> HowFuzzy
howFuzzy (Range (BoundaryBelow a) (BoundaryAbove b)) | d == 0 = ToTheDay
                                                     | d < 32 = ToTheMonth
                                                     | otherwise = ToTheYear
  where d = diffDays b a
