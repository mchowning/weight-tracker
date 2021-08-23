{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Averages where

import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import Data.Maybe (catMaybes)
import Data.Time (Day, utctDay)
import Data.Time.Calendar (toGregorian)
import Data.Text (Text)
import Types
import Data.Functor ((<&>))
import Date (shortPrettyPrintTime)
import Data.List.Split (divvy)
import qualified Data.Ord as Ord

data Row = Row
  { rowName :: Text,
    rowWeight :: Weight
  }

data FilledDay = FilledDay
  { filledDayDay :: Day,
    filledDayWeight :: Weight
  }

getMovingAverages :: Int -> [FilledDay] -> [Row]
getMovingAverages weeks es =
  movingAverages weeks es
    <&> \(_startDay, endDay, avg) ->
      Row
        { rowName = shortPrettyPrintTime endDay,
          rowWeight = avg
        }

movingAverages :: Int -> [FilledDay] -> [(Day, Day, Weight)]
movingAverages n es =
  let week = 7
      windowSize = week * n
      windowEveryNDays = week
      windows = divvy windowSize windowEveryNDays (reverse es)
      windowResult window = (filledDayDay (last window), filledDayDay (head window), Weight (average (fromWeight . filledDayWeight <$> window)))
   in windowResult <$> windows

average :: [Double] -> Double
average ds = precision 1 . (/ fromIntegral (length ds)) . sum $ ds

precision :: Int -> Double -> Double
precision i n = fromInteger (round (n * 10 ^ i)) / 10 ^ i

getMonthlyAverages :: [FilledDay] -> [Row]
getMonthlyAverages es =
  let averageMap = monthlyAverages es
      averageWeights = (\((y, m), w) -> (y, m, w)) <$> HM.toList averageMap
      sorted = L.sortOn (Ord.Down . byYear) . L.sortOn (Ord.Down . byMonth) $ averageWeights
      makeRow (year, month, weight) =
        Row
          { rowName = T.pack (show year) <> " " <> monthAbbrv month,
            rowWeight = weight
          }
   in makeRow <$> sorted
  where
    byYear (y, _, _) = y
    byMonth (_, m, _) = m
    monthAbbrv = \case
      1 -> "Jan"
      2 -> "Feb"
      3 -> "Mar"
      4 -> "Apr"
      5 -> "May"
      6 -> "Jun"
      7 -> "Jul"
      8 -> "Aug"
      9 -> "Sep"
      10 -> "Oct"
      11 -> "Nov"
      12 -> "Dec"
      m -> error ("Invalid month value: " <> show m)

-- Fills in missing days with the average weights from before and after that day
filledDays :: [Entry] -> [FilledDay]
filledDays entries =
  let earliestEntry = L.minimumBy (Ord.comparing entryDay) entries
      latestEntry = L.maximumBy (Ord.comparing entryDay) entries
      allDays = fillEntries earliestEntry latestEntry
      filledDays_ = fillKnownDays entries allDays
      leftFold = scanl1 leftFolder filledDays_
      rightFold = scanr1 (flip leftFolder) filledDays_
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

fillEntries :: Entry -> Entry -> [Day]
fillEntries earliest latest =
  let earliestDay = utctDay (entryDate earliest)
      lastDay = utctDay (entryDate latest)
   in enumFromTo earliestDay lastDay
