{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE

ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Main where

import AppUtils
import qualified Control.Foldl as Foldl
import Control.Monad.IO.Class (liftIO)
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Date
import DisplayAverages
import Text.Read (readMaybe)
import Turtle
  ( (<|>),
  )
import qualified Turtle
import Types
import Averages

main :: IO ()
main = do
  blankLine
  filename <- (<> "Dropbox/Exercise/weights.csv") <$> Turtle.home
  -- let filename = "weights.csv"
  saveWeightArg filename

  savedEntries :: [Entry] <- do
    lineLs <- Turtle.fold (Turtle.input filename) Foldl.list
    -- TODO would be more efficient to apply lineToEntry in the fold, but not sure how to do that with lineToEntry being impure
    sequence (lineToEntry <$> lineLs)

  displayFromLastWeek savedEntries

  let allDaysWithWeights = filledDays savedEntries
      movingAverageWeeks = NE.fromList [1, 2, 4]
      weeksText = T.intercalate "/" . fmap (T.pack . show) $ NE.toList movingAverageWeeks
  printMovingAverageGrids allDaysWithWeights movingAverageWeeks (weeksText <> " Week Trailing Averages")
  printMonthlyAverages allDaysWithWeights

saveWeightArg :: Turtle.FilePath -> IO ()
saveWeightArg fp = do
  maybeWeight <- Turtle.options "Daily Weight Tracker" do
    maybeDouble <- Turtle.optional (Turtle.argDouble "weight" "Weight entry for today (must be a number)")
    pure (Weight <$> maybeDouble)
  case maybeWeight of
    Nothing -> return ()
    Just weight -> do
      updateSavedWeights fp weight
      let showSavedWeight w = do
            putStrLn ("Weight of " <> showWeight w <> " successfully saved!")
            blankLine
      showSavedWeight weight

updateSavedWeights :: Turtle.FilePath -> Weight -> IO ()
updateSavedWeights path weight = do
  let orig = Turtle.input path
  let updated = orig <|> liftIO createEntryLine

  Turtle.output "/tmp/temp_weight.csv" updated
  Turtle.cp "/tmp/temp_weight.csv" path
  where
    createEntryLine :: IO Turtle.Line
    createEntryLine = do
      now <- getCurrentTimeText
      let line = now <> "," <> showWeight weight
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

lineToEntry :: MonadFail m => Turtle.Line -> m Entry
lineToEntry l = do
  let text = Turtle.lineToText l
  [dateText, weightText] <- case T.splitOn "," text of
    [a, b] -> return [a, b]
    _ -> fail ("Failed to parse Entry from line: " <> T.unpack text)
  date <- case readDate dateText of
    Just date -> return date
    Nothing -> fail ("Failed to parse date from line: " <> T.unpack text)
  weight <- case readWeight weightText of
    Just weight -> return weight
    Nothing -> fail ("Failed to parse weight from line: " <> T.unpack text)
  return (Entry date weight)
  where
    readWeight :: Text -> Maybe Weight
    readWeight = fmap Weight . readMaybe . T.unpack

-- data ReadEntryException = FailedToReadEntry Text
--                         | FailedToReadDate Text
--                         | FailedToReadWeight Text
-- instance Exception ReadEntryException

-- instance Show ReadEntryException where
--   show = \case
--     FailedToReadEntry t -> "Failed to parse Entry from line: " <> T.unpack t
--     FailedToReadDate t -> "Failed to parse date from line: " <> T.unpack t
--     FailedToReadWeight t -> "Failed to parse weight from line: " <> T.unpack t

-- lineToEntry :: (MonadFail m, MonadThrow m) => Turtle.Line -> m Entry
-- lineToEntry l = do
--   let text = Turtle.lineToText l
--   [dateText, weightText] <- case T.splitOn "," text of
--     [a, b] -> return [a, b]
--     _ -> throwM (FailedToReadEntry text)
--   date <- case readDate dateText of
--     Just date -> return date
--     Nothing -> throwM (FailedToReadDate text)
--   weight <- case readWeight weightText of
--     Just weight -> return weight
--     Nothing -> throwM (FailedToReadWeight text)
--   return (Entry date weight)
--   where
--     readWeight :: Text -> Maybe Weight
--     readWeight = fmap Weight . readMaybe . T.unpack
