{-# LANGUAGE TupleSections #-}

module Lib
  ( filledDays,
    monthlyAverages,
    movingAverages,
    Year,
    MonthOfYear,
    FilledDay (filledDayDay, filledDayWeight)
  )
where

import Data.Bifunctor (first)
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import Data.List.Split (divvy)
import Data.Maybe (catMaybes)
import Data.Ord (comparing)
import Data.Time (Day, utctDay)
import Data.Time.Calendar (toGregorian)
import Types

data FilledDay = FilledDay
  { filledDayDay :: Day,
    filledDayWeight :: Weight
  }

-- Fills in missing days with the average weights from before and after that day
filledDays :: [Entry] -> [FilledDay]
filledDays entries =
  let earliestEntry = L.minimumBy (comparing entryDay) entries
      latestEntry = L.maximumBy (comparing entryDay) entries
      allDays = fillEntries earliestEntry latestEntry
      filledDays = fillKnownDays entries allDays
      leftFold = scanl1 leftFolder filledDays
      rightFold = scanr1 (flip leftFolder) filledDays
      tups = catMaybes (zipWith zipper leftFold rightFold)
   in toFilledDay <$> tups
  where
    leftFolder ::
      (Day, Maybe Weight) -> (Day, Maybe Weight) -> (Day, Maybe Weight)
    leftFolder _ a@(_, Just _) = a
    leftFolder (_, w) (d, Nothing) = (d, w)

    zipper :: (Day, Maybe Weight) -> (Day, Maybe Weight) -> Maybe (Day, Weight)
    zipper (_, Nothing) _ = Nothing
    zipper _ (_, Nothing) = Nothing
    zipper (d1, Just (Weight w1)) (d2, Just (Weight w2)) =
      if d1 /= d2 then Nothing else Just . (d1,) . Weight $ (w1 + w2) / 2

    toFilledDay (day, weight) = FilledDay day weight

precision :: Int -> Double -> Double
precision i n = fromInteger (round (n * 10 ^ i)) / 10 ^ i

{-@ average :: { ds:[Double] | len ds > 0 } -> d:Double @-}
average :: [Double] -> Double
average ds = precision 1 . (/ fromIntegral (length ds)) . sum $ ds

-- FIXME: what if there are two weights for a day??
dayWeightMap :: [Entry] -> HM.HashMap DayInt Weight
dayWeightMap es =
  let tup = fmap (\e -> (fromEnum (utctDay (entryDate e)), entryWeight e)) es
   in HM.fromList tup

-- FIXME: using assume
{-@ assume fillKnownDays ::
    es : [Entry] ->
    ds : [Day] ->
    { rs : [(Day, Maybe Weight)] | len rs == len ds } @-}
-- {-@ fillKnownDays ::
--     { es : [Entry] | len es > 0 } ->
--     { ds : [Day] | len ds > 0 } ->
--     { rs : [(Day, Maybe Weight)] | len rs > 0 } @-}
fillKnownDays :: [Entry] -> [Day] -> [(Day, Maybe Weight)]
fillKnownDays es ds =
  let entryMap = dayWeightMap es
   in dayWithWeight entryMap <$> ds
  where
    dayWithWeight :: HM.HashMap DayInt Weight -> Day -> (Day, Maybe Weight)
    dayWithWeight dMap d = (d, HM.lookup (fromEnum d) dMap)

type Year = Integer

type MonthOfYear = Int

type DayOfMonth = Int

monthlyAverages :: [FilledDay] -> HM.HashMap (Year, MonthOfYear) Weight
monthlyAverages es = Weight . average . fmap fromWeight <$> grouped
  where
    expanded :: [((Year, MonthOfYear), Weight)]
    expanded = (\(FilledDay day weight) -> (monthYear day, weight)) <$> es

    monthYear :: Day -> (Year, MonthOfYear)
    monthYear = (\(y, m, _) -> (y, m)) . toGregorian

    grouped :: HM.HashMap (Year, MonthOfYear) [Weight]
    grouped = L.foldl' (\m (k, v) -> HM.insertWith (++) k [v] m) HM.empty expanded

movingAverages :: Int -> [FilledDay] -> [(Day, Day, Weight)]
movingAverages n es =
  let week = 7
      windowSize = week * n
      windowEveryNDays = week
      windows = divvy windowSize windowEveryNDays (reverse es)
      windowResult window = (filledDayDay (last window), filledDayDay (head window), Weight (average (fromWeight . filledDayWeight <$> window)))
   in windowResult <$> windows

fillEntries :: Entry -> Entry -> [Day]
fillEntries earliest latest =
  let earliestDay = utctDay (entryDate earliest)
      lastDay = utctDay (entryDate latest)
   in enumFromTo earliestDay lastDay
