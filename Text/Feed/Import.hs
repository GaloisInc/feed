--------------------------------------------------------------------
-- |
-- Module    : Text.Feed.Import
-- Copyright : (c) Galois, Inc. 2008
-- License   : BSD3
--
-- Maintainer: Don Stewart <dons@galois.com>
-- Stability : provisional
-- Portability:
--
--------------------------------------------------------------------

module Text.Feed.Import
        ( parseFeedFromFile -- :: FilePath -> IO Feed
        , parseFeedString   -- :: String -> IO Feed
        ) where

import Text.Atom.Feed.Import as Atom
import Text.RSS.Import       as RSS
import Text.RSS1.Import      as RSS1

import Text.Feed.Types
import Text.XML.Light as XML

import Control.Monad

parseFeedFromFile :: FilePath -> IO Feed
parseFeedFromFile fp = do
  ls <- readFile fp
  case parseFeedString ls of
    Nothing -> fail "parseFeedFromFile: not a well-formed XML content"
    Just f  -> return f

parseFeedString :: String -> Maybe Feed
parseFeedString str =
  case parseXMLDoc str of
    Nothing -> Nothing
    Just e  ->
      readAtom e `mplus`
      readRSS1 e `mplus`
      readRSS2 e `mplus`
      Just (XMLFeed e)

readRSS2 :: XML.Element -> Maybe Feed
readRSS2 e = fmap RSSFeed  $ RSS.elementToRSS e

readRSS1 :: XML.Element -> Maybe Feed
readRSS1 e = fmap RSS1Feed $ RSS1.elementToFeed e

readAtom :: XML.Element -> Maybe Feed
readAtom e = fmap AtomFeed $ Atom.elementFeed e

