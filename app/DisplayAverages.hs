{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module DisplayAverages (printMovingAverageGrids, printMonthlyAverages) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.List.NonEmpty (NonEmpty)
import Control.Monad
  ( forM_
  )

import AppUtils
import Types
import Grid
import Averages

newtype NumRows = NumRows Int

printMovingAverageGrids :: [FilledDay] -> NonEmpty Int -> Text -> IO ()
printMovingAverageGrids fds ns heading =
  let grid = movingAverageGrid fds ns
  in printGrid heading (take 12 grid)

printMonthlyAverages :: [FilledDay] -> IO ()
printMonthlyAverages ls = do
  let monthlyAvgs = getMonthlyAverages ls
  blankLine
  displayRows (NumRows 12) "Monthly Averages" monthlyAvgs
    where


displayRows :: NumRows -> Text -> [Row] -> IO ()
displayRows (NumRows numToDisplay) heading rs = do
  printHeading heading
  forM_ (take numToDisplay rs) \(Row name weight) ->
    let line = name <> ":  " <> T.pack (show (fromWeight weight))
     in TIO.putStrLn line


