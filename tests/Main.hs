module Main where

import Text.XML.Light
import Feed.Import
import Feed.Export

import System.Environment

main :: IO ()
main = do
  (x:_) <- getArgs
  feed  <- parseFeedFromFile x
  putStrLn (ppTopElement $ xmlFeed feed)
