module Types where

import Data.Time

data Entry = Entry
  { entryDate :: UTCTime,
    entryWeight :: Weight
  }
  deriving (Show)

type DayInt = Int

entryDay :: Entry -> Day
entryDay = utctDay . entryDate

newtype Weight = Weight Double deriving (Show)

fromWeight :: Weight -> Double
fromWeight (Weight w) = w

showWeight :: Weight -> String
showWeight (Weight w) = show w
