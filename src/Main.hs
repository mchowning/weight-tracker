{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Main where

import           Date                                     ( getCurrentTimeText
                                                          , readDate
                                                          , shortPrettyPrintTime
                                                          )

import qualified Control.Foldl                 as Foldl
import           Control.Monad.IO.Class                   ( liftIO )
import           Control.Monad                            ( forM_
                                                          , mzero
                                                          , when
                                                          )
import Data.Bifunctor                                     (first)
import qualified Data.HashMap.Strict           as HM
import qualified Data.List                     as L
import Data.List.Split (divvy)
import           Data.Maybe                               ( catMaybes
                                                          , mapMaybe
                                                          )
import           Data.Ord                                 ( comparing )
import           Data.Text                                ( Text )
import qualified Data.Text                     as T
import           Data.Time
import           Data.Time.Calendar
import           Text.Read                                ( readMaybe )
import           Turtle                                   ( Line
                                                          , FilePath
                                                          , Parser
                                                          , Shell
                                                          , argDouble
                                                          , cp
                                                          , fold
                                                          , home
                                                          , input
                                                          , lineToText
                                                          , optional
                                                          , output
                                                          , options
                                                          , sh
                                                          , unsafeTextToLine
                                                          , (<|>)
                                                          )

newtype Weight = Weight Double deriving Show

fromWeight :: Weight -> Double
fromWeight (Weight w) = w

main :: IO ()
main = do
  blankLine
  filename <- (<> "Dropbox/Exercise/weights.csv") <$> home
  -- let filename = "weights.csv"
  saveWeightArg filename

  savedWeights <- mapMaybe lineToEntry <$> fold (input filename) Foldl.list
  displayFromLastWeek savedWeights

  let allDaysWithWeights = filledDays savedWeights

  let weeksForMovingAverage = 4
  when (length allDaysWithWeights >= weeksForMovingAverage * 7)
    (do
      blankLine
      displayMovingAverages 10 weeksForMovingAverage allDaysWithWeights)

  blankLine
  displayMonthlyAverages allDaysWithWeights

blankLine :: IO ()
blankLine = putStrLn ""

saveWeightArg :: Turtle.FilePath -> IO ()
saveWeightArg fp = do
  maybeWeight <- parseArgs
  sequence_ (updateSavedWeights fp <$> maybeWeight)
  let showSavedWeight w = putStrLn ("Weight of " <> showWeight w <> " successfully saved!") >> blankLine
  sequence_ (showSavedWeight <$> maybeWeight)

updateSavedWeights :: Turtle.FilePath -> Weight -> IO ()
updateSavedWeights path weight = do
  let orig    = input path
  let updated = orig <|> liftIO (createEntryLine weight)
  output "/tmp/temp_weight.csv" updated
  cp "/tmp/temp_weight.csv" path

createEntryLine :: Weight -> IO Line
createEntryLine w = do
  now <- getCurrentTimeText
  let line = now <> "," <> showWeight w
  return . unsafeTextToLine . T.pack $ line

parseArgs :: IO (Maybe Weight)
parseArgs = options "Daily Weight Tracker" parser
 where
  parser :: Parser (Maybe Weight)
  parser =
    let parserMaybeDouble =
            optional (argDouble "weight" "Weight entry for today (must be a number)")
    in  (fmap . fmap) Weight parserMaybeDouble

showWeight :: Weight -> String
showWeight (Weight w) = show w

---------------------------------------------------------------------------

data Entry = Entry { entryDate :: UTCTime
                   , entryWeight :: Weight } deriving Show

type DayInt = Int

entryDay :: Entry -> Day
entryDay = utctDay . entryDate

prettyPrintEntry :: Entry -> String
prettyPrintEntry (Entry time (Weight w)) =
  shortPrettyPrintTime time <> ": " <> show w

displayFromLastWeek :: [Entry] -> IO ()
displayFromLastWeek es = do
  let heading = "Entries from the past week"
  today <- utctDay <$> getCurrentTime
  let last8Days = fromLastNDays 8 today es
  displayEntries heading (reverse last8Days)

fromLastNDays :: Integer -> Day -> [Entry] -> [Entry]
fromLastNDays n day = 
  filter ((< n) . diffDays day . utctDay . entryDate)

displayEntries :: String -> [Entry] -> IO ()
displayEntries heading es = do
  printHeading heading
  forM_ es (putStrLn . prettyPrintEntry)

printHeading :: String -> IO ()
printHeading heading = do
  putStrLn heading 
  putStrLn (replicate (length heading) '=')

displayLastN :: Int -> [Entry] -> IO ()
displayLastN n es = 
  let heading = "Last " <> show n <> " Entries"
      lastN = take n (reverse es)
  in displayEntries heading lastN

-- {-@ type OrdDayList d = [d]<{\d1 d2 -> d1 > d2}> @-}
-- {-@ orderedDayList :: OrdDayList Day @-}
-- orderedDayList :: [Day]
-- orderedDayList = ModifiedJulianDay <$> [5,4..1]


-- Fills in missing days with the average weights from before and after that day
filledDays :: [Entry] -> [(Day, Weight)]
filledDays entries = 
  let earliestEntry = L.minimumBy (comparing entryDay) entries
      latestEntry   = L.maximumBy (comparing entryDay) entries
      allDays       = fillEntries earliestEntry latestEntry
      filledDays    = fillKnownDays entries allDays
      leftFold      = scanl1 leftFolder filledDays
      rightFold     = scanr1 (flip leftFolder) filledDays
  in catMaybes (zipWith zipper leftFold rightFold)
 where
  leftFolder
    :: (Day, Maybe Weight) -> (Day, Maybe Weight) -> (Day, Maybe Weight)
  leftFolder _      a@(_, Just _ ) = a
  leftFolder (_, w) (  d, Nothing) = (d, w)

  zipper :: (Day, Maybe Weight) -> (Day, Maybe Weight) -> Maybe (Day, Weight)
  zipper (_, Nothing) _            = Nothing
  zipper _            (_, Nothing) = Nothing
  zipper (d1, Just (Weight w1)) (d2, Just (Weight w2)) =
    if d1 /= d2 then Nothing else Just . (d1, ) . Weight $ (w1 + w2) / 2

precision :: Int -> Double -> Double
precision i n = fromInteger (round (n * 10^i)) / 10^i

{-@ average :: { ds:[Double] | len ds > 0 } -> d:Double @-}
average :: [Double] -> Double
average ds = precision 1 . (/ fromIntegral (length ds)) . sum $ ds
-- average ds = if null ds then 0 else precision 1 . (/ fromIntegral (length ds)) . sum $ ds
-- average ds = if length ds == 0 then 0 else precision 1 . (/ fromIntegral (length ds)) . sum $ ds

-- FIXME: what if there are two weights for a day??
dayWeightMap :: [Entry] -> HM.HashMap DayInt Weight
dayWeightMap es =
  let tup = fmap (\e -> (fromEnum (utctDay (entryDate e)), entryWeight e)) es
  in  HM.fromList tup


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
  let entryMap    = dayWeightMap es
  in dayWithWeight entryMap <$> ds
 where
  dayWithWeight :: HM.HashMap DayInt Weight -> Day -> (Day, Maybe Weight)
  dayWithWeight dMap d = (d, HM.lookup (fromEnum d) dMap)

{-@ displayMovingAverages :: 
  { weeks : Int | weeks > 0 } -> 
  ls : [(Day, Weight)] -> 
  io : IO () @-}
displayMovingAverages :: Int -> Int -> [(Day, Weight)] -> IO ()
displayMovingAverages numToDisplay weeks es = do
  printHeading ("Trailing " <> show weeks <> " Week Averages")
  let averages = movingAverages weeks es
      -- everyOther xs = [ fst x | x <- zip xs [1..], odd (snd x)]
      -- averagesToPrint = everyOther . take numToDisplay $ averages
      averagesToPrint = take numToDisplay $ averages
  forM_ averagesToPrint (\(_, end, avg) ->
    putStrLn (shortPrettyPrintTime end <> ": " <> show avg))

displayMonthlyAverages :: [(Day, Weight)] -> IO ()
displayMonthlyAverages es = do
  printHeading "Monthly Averages"
  let averageMap = monthlyAverages es
      sorted :: [(Year, MonthOfYear, Weight)]
      year (y,_,_) = y
      month (_,m,_) = m
      sorted = (reverse . L.sortOn year . L.sortOn month) $ (\((y, m), w) -> (y, m, w)) <$> HM.toList averageMap
  forM_ sorted (\(year, month, avg) ->
    -- putStrLn (show year <> "-" <> pad 2 (show month) <> ": " <> show (fromWeight avg)))
    putStrLn (show year <> " " <> monthAbbrv month <> ": " <> show (fromWeight avg)))
  where
      year (y,_,_) = y
      month (_,m,_) = m
      pad n str = replicate (n - length str) '0' ++ str
      monthAbbrv 1 = "Jan"
      monthAbbrv 2 = "Feb"
      monthAbbrv 3 = "Mar"
      monthAbbrv 4 = "Apr"
      monthAbbrv 5 = "May"
      monthAbbrv 6 = "Jun"
      monthAbbrv 7 = "Jul"
      monthAbbrv 8 = "Aug"
      monthAbbrv 9 = "Sep"
      monthAbbrv 10 = "Oct"
      monthAbbrv 11 = "Nov"
      monthAbbrv 12 = "Dec"

type Year = Integer
type MonthOfYear = Int
type DayOfMonth = Int

monthlyAverages :: [(Day, Weight)] -> HM.HashMap (Year, MonthOfYear) Weight
monthlyAverages es = (Weight . average . fmap fromWeight) <$> grouped
  -- (\((y,m,_), w) -> (y,m,w)) <$> (expand (L.sortOn fst es))
 where
  expanded :: [((Year, MonthOfYear), Weight)]
  expanded = first monthYear <$> es

  monthYear :: Day -> (Year, MonthOfYear)
  monthYear = (\(y,m,_) -> (y,m)) . toGregorian

  grouped :: HM.HashMap (Year, MonthOfYear) [Weight]
  grouped = L.foldl' (\m (k,v) -> HM.insertWith (++) k [v] m) HM.empty expanded

-- {-@ assume divvy ::
--     a : Nat ->
--     b : Nat ->
--     cs :[c] ->
--     { rs : [[c]] | len rs = div ((len cs) - a + b) a } @-} -- length of list and length of all elements in list

{-@ movingAverages :: 
    { n : Int | n > 0 } -> 
    { ts : [(Day, Weight)] | len ts >= n * 7 } -> 
    rs : [(Day, Day, Double)] @-}
    -- { rs : [(Day, Day, Double)] | len rs = div (len ts) n } @-}
movingAverages :: Int -> [(Day, Weight)] -> [(Day, Day, Double)]
movingAverages n es =
  let week = 7
      windowEveryNDays = week
      windowSize = week * n
      windows = divvy windowSize windowEveryNDays (reverse es)
      windowResult window = (fst (last window), fst (head window), average (fromWeight . snd <$> window))
  in windowResult <$> windows

-- fillEntries :: Entry -> Entry -> [Entry] -> [Entry]
{-@ fillEntries :: e1 : Entry -> e2 : Entry -> { ds : [Day] | len ds > 0 } @-}
fillEntries :: Entry -> Entry -> [Day]
fillEntries earliest latest =
  let earliestDay = utctDay (entryDate earliest)
      lastDay     = utctDay (entryDate latest)
  in  enumFromTo earliestDay lastDay

linesToEntries :: Shell Line -> Shell Entry
linesToEntries sLines = do
  mEntry <- lineToEntry <$> sLines
  maybe mzero return mEntry

-- FIXME: Need some kind of notification coming out of this that there was a parse error (and whether 
--   the error was on the weight or the date parsing)
lineToEntry :: Line -> Maybe Entry
lineToEntry l =
  let text = lineToText l
  in  case T.splitOn "," text of
        [date, weight] -> Entry <$> readDate date <*> readWeight weight
        _              -> Nothing
 where
  readWeight :: Text -> Maybe Weight
  readWeight = fmap Weight . readMaybe . T.unpack
