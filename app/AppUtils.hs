{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
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
import Averages
import Data.Functor ((<&>))
import System.Console.ANSI

newtype NumRows = NumRows Int

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
  let rows = es <&> \(Entry date weight) -> Row {
        rowName = shortPrettyPrintTime date,
        rowWeight = weight
                                                }
  displayRows (NumRows (length rows)) heading rows
  -- printHeading heading
  -- forM_ es (TIO.putStrLn . prettyPrintEntry)

  -- shortPrettyPrintTime time <> ": " <> T.pack (show w)

displayRows :: NumRows -> Text -> [Row] -> IO ()
displayRows (NumRows numToDisplay) heading rs = do
  printHeading heading
  forM_ (take numToDisplay rs) \(Row name weight) -> do
    setSGR emphasizeSGR
    TIO.putStr (name <> ":  ")
    setSGR [Reset]
    print (fromWeight weight)

emphasizeSGR :: [SGR]
emphasizeSGR = [SetConsoleIntensity BoldIntensity, SetItalicized True]
