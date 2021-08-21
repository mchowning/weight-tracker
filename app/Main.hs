{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Control.Foldl as Foldl
import Control.Monad
  ( forM_,
    when,
  )
import Control.Monad.IO.Class (liftIO)
import Data.Functor
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import qualified Data.Ord as Ord
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time
import Date
import Lib
import Text.Read (readMaybe)
import Turtle
  ( (<|>),
  )
import qualified Turtle
import qualified Data.List.NonEmpty as NE

import AppUtils
import DisplayAverages
import Types
-- import Grid

main :: IO ()
main = do
  blankLine
  filename <- (<> "Dropbox/Exercise/weights.csv") <$> Turtle.home
  -- let filename = "weights.csv"
  saveWeightArg filename

  savedEntries <- do
    lines <- Turtle.fold (Turtle.input filename) Foldl.list
    return (lineToEntry <$> lines)
  displayFromLastWeek savedEntries

  let allDaysWithWeights = filledDays savedEntries
      movingAverageWeeks = NE.fromList [1, 2, 4]
      weeksText = T.intercalate "/" . fmap (T.pack . show) $ NE.toList movingAverageWeeks
  printMovingAverageGrids allDaysWithWeights movingAverageWeeks (weeksText <> " Week Trailing Averages")
  printMonthlyAverages allDaysWithWeights

saveWeightArg :: Turtle.FilePath -> IO ()
saveWeightArg fp = do
  maybeWeight <- parseArgs
  sequence_ (updateSavedWeights fp <$> maybeWeight)
  let showSavedWeight w = do
        putStrLn ("Weight of " <> showWeight w <> " successfully saved!")
        blankLine
  sequence_ (showSavedWeight <$> maybeWeight)
    where
      parseArgs :: IO (Maybe Weight)
      parseArgs = Turtle.options "Daily Weight Tracker" parser

      parser :: Turtle.Parser (Maybe Weight)
      parser =
        let parserMaybeDouble =
              Turtle.optional (Turtle.argDouble "weight" "Weight entry for today (must be a number)")
        in (fmap . fmap) Weight parserMaybeDouble

updateSavedWeights :: Turtle.FilePath -> Weight -> IO ()
updateSavedWeights path weight = do
  let orig = Turtle.input path
  let updated = orig <|> liftIO (createEntryLine weight)

  Turtle.output "/tmp/temp_weight.csv" updated
  Turtle.cp "/tmp/temp_weight.csv" path
    where
      createEntryLine :: Weight -> IO Turtle.Line
      createEntryLine w = do
        now <- getCurrentTimeText
        let line = now <> "," <> showWeight w
        return . Turtle.unsafeTextToLine . T.pack $ line


displayFromLastWeek :: [Entry] -> IO ()
displayFromLastWeek es = do
  let heading = "Entries from the past week"
  today <- utctDay <$> getCurrentTime
  let last8Days = fromLastNDays 8 today es
  displayEntries heading (reverse last8Days)
    where
      fromLastNDays :: Integer -> Day -> [Entry] -> [Entry]
      fromLastNDays n day =
        filter ((< n) . diffDays day . utctDay . entryDate)

lineToEntry :: Turtle.Line -> Entry
lineToEntry l =
  let text = Turtle.lineToText l
   in case T.splitOn "," text of
        [dateText, weightText] ->
          let date = case readDate dateText of
                Nothing -> error ("Failed to parse date from line: " <> T.unpack text)
                Just date -> date
              weight = case readWeight weightText of
                Nothing -> error ("Failed to parse weight from line: " <> T.unpack text)
                Just weight -> weight
           in Entry date weight
        _ -> error ("Failed to parse Entry from line: " <> T.unpack text)
  where
    readWeight :: Text -> Maybe Weight
    readWeight = fmap Weight . readMaybe . T.unpack
