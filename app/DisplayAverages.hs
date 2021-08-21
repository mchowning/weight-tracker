{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module DisplayAverages (printMovingAverageGrids, printMonthlyAverages) where

import Data.Time
import Data.Text (Text)
import Data.Functor
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.HashMap.Strict as HM
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List as L
import qualified Data.Ord as Ord
import Control.Monad
  ( forM_,
    when
  )

import AppUtils
import Types
import Lib
import Date
import Grid
import Averages

newtype NumRows = NumRows Int

printMovingAverageGrids :: [FilledDay] -> NonEmpty Int -> Text -> IO ()
printMovingAverageGrids fds ns heading =
  let grid = movingAverageGrid fds ns
  in printGrid heading (take 12 grid)

printMonthlyAverages :: [FilledDay] -> IO ()
printMonthlyAverages ls = do
  let monthlyAverages = getMonthlyAverages ls
  blankLine
  displayRows (NumRows 12) "Monthly Averages" monthlyAverages
    where

getMonthlyAverages :: [FilledDay] -> [Row]
getMonthlyAverages es =
  let averageMap = monthlyAverages es
      averageWeights = (\((y, m), w) -> (y, m, w)) <$> HM.toList averageMap
      sorted = L.sortOn (Ord.Down . year) . L.sortOn (Ord.Down . month) $ averageWeights
      makeRow (year, month, weight) =
        Row
          { rowName = T.pack (show year) <> " " <> monthAbbrv month,
            rowWeight = weight
          }
   in makeRow <$> sorted
  where
    year (y, _, _) = y
    month (_, m, _) = m
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

displayRows :: NumRows -> Text -> [Row] -> IO ()
displayRows (NumRows numToDisplay) heading rs = do
  printHeading heading
  forM_ (take numToDisplay rs) \(Row name weight) ->
    let line = name <> ":  " <> T.pack (show (fromWeight weight))
     in TIO.putStrLn line


