{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module DisplayAverages (printMovingAverageGrids, printMonthlyAverages) where

import Data.Text (Text)
import Data.List.NonEmpty (NonEmpty)

import AppUtils
import Grid
import Averages

printMovingAverageGrids :: [FilledDay] -> NonEmpty Int -> Text -> IO ()
printMovingAverageGrids fds ns heading =
  let grid = movingAverageGrid fds ns
  in printGrid heading (take 12 grid)

printMonthlyAverages :: [FilledDay] -> IO ()
printMonthlyAverages ls = do
  let monthlyAvgs = getMonthlyAverages ls
  blankLine
  displayRows (NumRows 12) "Monthly Averages" monthlyAvgs
