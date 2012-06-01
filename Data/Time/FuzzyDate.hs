{-# LANGUAGE OverloadedStrings #-}
-- |FuzzyDate is useful for occasions when it is unknown to what
--  precision dates will be specified. It can handle cases where:
--  * only the year is known
--  * both the month and year are known
--  * the day, month and year are all known
module Data.Time.FuzzyDate
       ( fuzzyDateValid
       , toText
       , HowFuzzy(..)
       , howFuzzy
       ) where

import qualified Data.Time.Calendar as New
import qualified Data.Time.Calendar.OrdinalDate as Ordinal
import qualified Data.Time.Format as Format
import qualified System.Time as Old
import Data.Text (Text)
import Data.Maybe
import Data.Time.FuzzyDate.Private

data FuzzyDate = KnownDay New.Day
               | KnownMonth Integer Old.Month
               | KnownYear Integer
               deriving (Eq, Show)

-- |Expects year, then month, then day, where any or all of them may
--  be unavailable.
fuzzyDateValid :: Maybe Integer -> Maybe Int -> Maybe Int -> Maybe FuzzyDate
fuzzyDateValid (Just y) (Just m) (Just d) = Just . KnownDay =<< New.fromGregorianValid y m d
fuzzyDateValid (Just y) (Just m) _ = Just . KnownMonth y =<< monthValid m
fuzzyDateValid (Just y) _ _ = Just $ KnownYear y
fuzzyDateValid _ _ _ = Nothing

monthValid :: Int -> Maybe Old.Month
monthValid i | i' < fromEnum (minBound :: Old.Month) = Nothing
             | i' > fromEnum (maxBound :: Old.Month) = Nothing
             | otherwise = Just $ toEnum i'
  where i' = i - 1

toText :: FuzzyDate -> Text
toText (KnownDay d) = undefined

toYear :: FuzzyDate -> Integer
toYear (KnownDay d) = fst $ Ordinal.toOrdinalDate d
toYear (KnownMonth y _) = y
toYear (KnownYear y) = y

toMonth :: FuzzyDate -> Int
toMonth (KnownDay d) = (\(_,m,_) -> m) $ New.toGregorian d
toMonth (KnownMonth _ m) = (+1) $ fromEnum  m
toMonth (KnownYear _) = 0

toDayOfMonth :: FuzzyDate -> Int
toDayOfMonth (KnownDay d) = (\(_,_,i) -> i) $ New.toGregorian d
toDayOfMonth _ = 0

toDayOfYear :: FuzzyDate -> Int
toDayOfYear (KnownDay d) = snd $ Ordinal.toOrdinalDate d
toDayOfYear _ = 0

data HowFuzzy = ToTheDay | ToTheMonth | ToTheYear deriving (Eq, Show)

howFuzzy :: FuzzyDate -> HowFuzzy
howFuzzy (KnownDay _) = ToTheDay
howFuzzy (KnownMonth _ _) = ToTheMonth
howFuzzy (KnownYear _) = ToTheYear

-- Not complete!
instance Format.FormatTime FuzzyDate where
  -- Aggregate
  formatCharacter 'D' = Just (\locale _ -> Format.formatTime locale "%m/%d/%y")
  formatCharacter 'F' = Just (\locale _ -> Format.formatTime locale "%Y-%m-%d")
  --formatCharacter 'x' = Just (\locale _ -> formatTime locale (dateFmt locale))

  -- Year Count
  formatCharacter 'Y' = Just (\_ _ -> show . toYear)
  formatCharacter 'y' = Just (\_ opt -> (show2 $ zeroOr opt) . mod100 . toYear)
  --formatCharacter 'C' = Just (\_ opt -> (show2 (fromMaybe (Just '0') opt)) . div100 . fst . toOrdinalDate)
  -- Month of Year
  --formatCharacter 'B' = Just (\locale _ -> fst . (\(_,m,_) -> (months locale) !! (m - 1)) . toGregorian)
  --formatCharacter 'b' = Just (\locale _ -> snd . (\(_,m,_) -> (months locale) !! (m - 1)) . toGregorian)
  --formatCharacter 'h' = Just (\locale _ -> snd . (\(_,m,_) -> (months locale) !! (m - 1)) . toGregorian)
  formatCharacter 'm' = Just (\_ opt -> (show2 $ zeroOr opt) . toMonth)
  -- Day of Month
  formatCharacter 'd' = Just (\_ opt -> (show2 $ zeroOr opt) . toDayOfMonth)
  formatCharacter 'e' = Just (\_ opt -> (show2 $ spaceOr opt) . toDayOfMonth)
  -- Day of Year
  formatCharacter 'j' = Just (\_ opt -> (show3 $ zeroOr opt) . toDayOfYear)

  -- ISO 8601 Week Date
  --formatCharacter 'G' = Just (\_ _ -> show . (\(y,_,_) -> y) . toWeekDate)
  --formatCharacter 'g' = Just (\_ opt -> (show2 (fromMaybe (Just '0') opt)) . mod100 . (\(y,_,_) -> y) . toWeekDate)
  --formatCharacter 'f' = Just (\_ opt -> (show2 (fromMaybe (Just '0') opt)) . div100 . (\(y,_,_) -> y) . toWeekDate)

  --formatCharacter 'V' = Just (\_ opt -> (show2 (fromMaybe (Just '0') opt)) . (\(_,w,_) -> w) . toWeekDate)
  --formatCharacter 'u' = Just (\_ _ -> show . (\(_,_,d) -> d) . toWeekDate)

  -- Day of week
  --formatCharacter 'a' = Just (\locale _ -> snd . ((wDays locale) !!) . snd . sundayStartWeek)
  --formatCharacter 'A' = Just (\locale _ -> fst . ((wDays locale) !!) . snd . sundayStartWeek)
  --formatCharacter 'U' = Just (\_ opt -> (show2 (fromMaybe (Just '0') opt)) . fst . sundayStartWeek)
  --formatCharacter 'w' = Just (\_ _ -> show . snd . sundayStartWeek)
  --formatCharacter 'W' = Just (\_ opt -> (show2 (fromMaybe (Just '0') opt)) . fst . mondayStartWeek)

  -- Default
  formatCharacter _   = Nothing
