{-# LANGUAGE OverloadedStrings #-}
module AppUtils where

import Date
  ( shortPrettyPrintTime,
  )
import Types
import Control.Monad
  ( forM_ )
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

blankLine :: IO ()
blankLine = putStrLn ""

prettyPrintEntry :: Entry -> Text
prettyPrintEntry (Entry time (Weight w)) =
  shortPrettyPrintTime time <> ": " <> T.pack (show w)

printHeading :: Text -> IO ()
printHeading heading = do
  let paddedHeading = "  " <> heading <> "  "
      line = T.replicate (T.length paddedHeading) "="
  TIO.putStrLn line
  TIO.putStrLn paddedHeading
  TIO.putStrLn line

displayEntries :: Text -> [Entry] -> IO ()
displayEntries heading es = do
  printHeading heading
  forM_ es (TIO.putStrLn . prettyPrintEntry)
