module Feed.Import 
        ( parseFeedFromFile -- :: FilePath -> IO Feed
	, parseFeedString   -- :: String -> IO Feed
	) where

import Atom.Feed.Import as Atom
import RSS.Import       as RSS
import RSS1.Import      as RSS1

import Feed.Types
import XML 

import Control.Monad

parseFeedFromFile :: FilePath -> IO Feed
parseFeedFromFile fp = readFile fp >>= parseFeedString

parseFeedString :: String -> IO Feed
parseFeedString str = do
  case parseXMLDoc str of
    Nothing -> fail "parseFeedString: not a well-formed XML content"
    Just e  -> 
      case readAtom e `mplus` readRSS1 e `mplus` readRSS2 e of
        Nothing -> return (XMLFeed e)
	Just f  -> return f
    
   
readRSS2 :: XML.Element -> Maybe Feed
readRSS2 e = fmap RSSFeed  $ RSS.elementToRSS e

readRSS1 :: XML.Element -> Maybe Feed
readRSS1 e = fmap RSS1Feed $ RSS1.elementToFeed e

readAtom :: XML.Element -> Maybe Feed
readAtom e = fmap AtomFeed $ Atom.elementFeed e

