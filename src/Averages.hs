{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Averages where

import Data.Text (Text)
import Types (Weight)
import Lib
import Data.Functor ((<&>))
import Date (shortPrettyPrintTime)

data Row = Row
  { rowName :: Text,
    rowWeight :: Weight
  }

getMovingAverages :: Int -> [FilledDay] -> [Row]
getMovingAverages weeks es =
  movingAverages weeks es
    <&> \(_startDay, endDay, avg) ->
      Row
        { rowName = shortPrettyPrintTime endDay,
          rowWeight = avg
        }
