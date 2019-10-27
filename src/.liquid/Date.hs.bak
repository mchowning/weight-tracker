{-# LANGUAGE OverloadedStrings #-}

module Date (getCurrentTimeText, readDate, shortPrettyPrintTime) where

import           Data.Time
import           qualified Data.Text as T

getCurrentTimeText :: IO String
getCurrentTimeText = formatTime defaultTimeLocale dateFormat <$> getCurrentTime

readDate :: T.Text -> Maybe UTCTime
readDate =  parseTimeM False defaultTimeLocale dateFormat . T.unpack

dateFormat :: String
dateFormat = iso8601DateFormat (Just "%H:%M")

shortPrettyPrintTime :: FormatTime t => t -> String
shortPrettyPrintTime = formatTime defaultTimeLocale "%a, %b %d"
