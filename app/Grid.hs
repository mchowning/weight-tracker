{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Grid where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.List as L
import Types
import Data.Functor ((<&>))
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty)
import Control.Monad (forM_)
import AppUtils
import Averages
import System.Console.ANSI
import GHC.Base (when)

type Grid = [[Text]]

movingAverageGrid ::
  [FilledDay] -- ^ Weights for every day
  -> NonEmpty Int -- ^ Trailing week averages to include in grid
  -> Grid
movingAverageGrid fds ns =
  let weightColumns :: NonEmpty (Int, [(Text, Weight)]) = do
        n <- ns
        let dayWeightTups = getMovingAverages n fds <&> \(Row day weight) -> (day, weight)
        return (n, dayWeightTups)
      dayColumn :: [Text] = "" : -- Add empty text for "header"
        (fmap fst . -- just want the "days"
         snd . -- don't care about the length of the trailing average
         NE.head $ -- First column has the smallest trailing average, which will result in the "longest" column
         weightColumns)
      fullWeightColumns = do
        col <- weightColumns
        let numWeekAvg = T.pack . show . fst $ col
            firstRow = numWeekAvg <> " Week"
            remainingRows = T.pack . showWeight . snd <$> snd col
        return (firstRow : remainingRows)
   in L.transpose (dayColumn : NE.toList fullWeightColumns)

printGrid :: Text -> Grid -> IO ()
printGrid heading g = do
  let widest = maximum . fmap T.length . concat $ g
      padded = (fmap . fmap) (T.center widest ' ') g
  blankLine
  printHeading heading
  forM_ (zip [(0 :: Int) ..] padded) \(rowIdx, row) -> do
    forM_ (zip [(0 :: Int) ..] row) \(colIdx, cell) -> do
      let emphasize = rowIdx == 0 || colIdx == 0
      when emphasize (setSGR emphasizeSGR)
      TIO.putStr cell
      when emphasize (setSGR [Reset])
    blankLine
