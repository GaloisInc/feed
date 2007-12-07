module Main where

import XML
import RSS.Import
import RSS.Export
import RSS.Syntax

import RSS1.Import as RSS1
import RSS1.Export as RSS1

import System.Environment
import Data.Maybe
import Control.Monad

readRSS2 :: XML.Element -> Maybe String
readRSS2 e = fmap (ppTopElement . xmlRSS) $ elementToRSS e

readRSS1 :: XML.Element -> Maybe String
readRSS1 e = fmap (ppTopElement . RSS1.xmlFeed) $ RSS1.elementToFeed e

main :: IO ()
main = do
  (x:_) <- getArgs
  ls <- readFile x
  case parseXMLDoc ls of
    Nothing -> putStrLn "no parse"
    Just d  -> 
      case readRSS1 d `mplus` readRSS2 d of
        Nothing -> putStrLn "not well-formed RSS input"
	Just s  -> do
	  putStrLn "Parsed RSS input OK"
	  putStrLn s
