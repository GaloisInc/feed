module Feed.Constructor 
       ( FeedKind(..)
       , newFeed       -- :: FeedKind -> Feed
       , getFeedKind   -- :: Feed     -> FeedKind
       , addItem       -- :: Item -> Feed -> Feed

       , newItem       -- :: FeedKind   -> Item
       , withItemTitle -- :: String     -> Item -> Item
       , withItemLink  -- :: URLString  -> Item -> Item
       , withItemDate  -- :: DateString -> Item -> Item
       ) where

import Feed.Types

import Atom.Feed as Atom
import RSS.Syntax as RSS
import RSS1.Syntax as RSS1
import DublinCore.Types
import Text.XML.Light as XML

import Data.Maybe ( fromMaybe, mapMaybe )

data FeedKind
 = AtomKind
 | RSSKind (Maybe String)  -- Nothing => default version (2.0)
 | RDFKind (Maybe String)  -- Nothing => default version (1.0)
   deriving ( Eq )

newFeed :: FeedKind -> Feed.Types.Feed
newFeed fk = 
  case fk of
    AtomKind -> AtomFeed (Atom.nullFeed "feed-id-not-filled-in"
			                (TextString "dummy-title")
				        "dummy-and-bogus-update-date")
    RSSKind mbV -> 
      let def = (RSS.nullRSS "dummy-title" "default-channel-url") in
      RSSFeed $ fromMaybe def $ fmap (\ v -> def{RSS.rssVersion=v}) mbV
    RDFKind mbV -> 
      let def = (RSS1.nullFeed "default-channel-url" "dummy-title") in
      RSS1Feed $ fromMaybe def $ fmap (\ v -> def{RSS1.feedVersion=v}) mbV
      
    
getFeedKind :: Feed.Types.Feed -> FeedKind
getFeedKind f = 
  case f of
    Feed.Types.AtomFeed{} -> AtomKind
    Feed.Types.RSSFeed r  -> RSSKind (case RSS.rssVersion r of { "2.0" -> Nothing; v -> Just v})
    Feed.Types.RSS1Feed r -> RDFKind (case RSS1.feedVersion r of { "1.0" -> Nothing; v -> Just v})
    Feed.Types.XMLFeed{}  -> RSSKind (Just "2.0") -- for now, just a hunch..

addItem :: Feed.Types.Item -> Feed.Types.Feed -> Feed.Types.Feed
addItem it f = 
  case (it,f) of
    (Feed.Types.AtomItem e, Feed.Types.AtomFeed fe) -> 
       Feed.Types.AtomFeed fe{Atom.feedEntries=e:Atom.feedEntries fe}
    (Feed.Types.RSSItem e, Feed.Types.RSSFeed r) -> 
       Feed.Types.RSSFeed r{RSS.rssChannel=(RSS.rssChannel r){RSS.rssItems=e:RSS.rssItems (RSS.rssChannel r)}}
    (Feed.Types.RSS1Item e, Feed.Types.RSS1Feed r) -> 
    	 -- note: do not update the channel item URIs at this point;
	 -- will delay doing so until serialization.
       Feed.Types.RSS1Feed r{RSS1.feedItems=e:RSS1.feedItems r}
    _ -> error "addItem: currently unable to automatically convert items from one feed type to another"

newItem :: FeedKind -> Feed.Types.Item
newItem fk = 
  case fk of
    AtomKind -> 
               Feed.Types.AtomItem (Atom.nullEntry "entry-id-not-filled-in"
                                                    (TextString "dummy-entry-title")
						    "dummy-and-bogus-entry-update-date")
    RSSKind{} -> 
    	       Feed.Types.RSSItem (RSS.nullItem "dummy-rss-item-title")
    RDFKind{} -> 
               Feed.Types.RSS1Item (RSS1.nullItem "dummy-item-uri"
						  "dummy-item-title"
						  "dummy-item-link")

-- | 'withItemDate updDate' associates the last-updated date, 'updDate',
-- with a feed item.
withItemDate :: DateString -> Feed.Types.Item -> Feed.Types.Item
withItemDate dt fi = 
  case fi of
    Feed.Types.AtomItem e ->
      Feed.Types.AtomItem e{Atom.entryUpdated=dt}
    Feed.Types.RSSItem i  ->
      Feed.Types.RSSItem  i{RSS.rssItemPubDate=Just dt}
    Feed.Types.RSS1Item i ->
      case break isDate $ RSS1.itemDC i of
       (as,(dci:bs)) -> Feed.Types.RSS1Item i{RSS1.itemDC=as++dci{dcText=dt}:bs}
       (_,[]) -> Feed.Types.RSS1Item i{RSS1.itemDC=DCItem{dcElt=DC_Date,dcText=dt}:RSS1.itemDC i}
    Feed.Types.XMLItem i  ->
      Feed.Types.XMLItem $
        addChild (leaf (unqual "pubDate") dt) $
	    filterChildren (\ e -> elName e /= unqual "pubDate")
	                   i
 where
  isDate dc  = dcElt dc == DC_Date

-- | 'withItemTitle myTitle' associates a new title, 'myTitle',
-- with a feed item.
withItemTitle :: String -> Feed.Types.Item -> Feed.Types.Item
withItemTitle tit fi = 
  case fi of
    Feed.Types.AtomItem e ->
      Feed.Types.AtomItem e{Atom.entryTitle=TextString tit}
    Feed.Types.RSSItem i  ->
      Feed.Types.RSSItem  i{RSS.rssItemTitle=Just tit}
    Feed.Types.RSS1Item i ->
      Feed.Types.RSS1Item  i{RSS1.itemTitle=tit}
    Feed.Types.XMLItem i  ->
      Feed.Types.XMLItem $
        addChild (leaf (unqual "title") tit) $
	    filterChildren (\ e -> elName e /= unqual "title")
	                   i

-- | 'withItemTitle myLink' associates a new URL, 'myLink',
-- with a feed item.
withItemLink :: URLString -> Feed.Types.Item -> Feed.Types.Item
withItemLink url fi = 
  case fi of
    Feed.Types.AtomItem e ->
      Feed.Types.AtomItem e{Atom.entryLinks=replaceAlternate url (Atom.entryLinks e)}
    Feed.Types.RSSItem i  ->
      Feed.Types.RSSItem  i{RSS.rssItemLink=Just url}
    Feed.Types.RSS1Item i ->
      Feed.Types.RSS1Item  i{RSS1.itemLink=url}
    Feed.Types.XMLItem i  ->
      Feed.Types.XMLItem $
        addChild (leaf (unqual "link") url) $
	    filterChildren (\ e -> elName e /= unqual "link")
	                   i
 where
  replaceAlternate _ [] = []
  replaceAlternate x (lr:xs) 
   | toStr (Atom.linkRel lr) == "alternate" = lr{Atom.linkHref=x} : xs
   | otherwise = lr : replaceAlternate x xs

  toStr Nothing = ""
  toStr (Just (Left x)) = x
  toStr (Just (Right x)) = x
    
-- helpers..

filterChildren :: (XML.Element -> Bool) -> XML.Element -> XML.Element
filterChildren pre e = 
  case elContent e of
    [] -> e
    cs -> e { elContent = mapMaybe filterElt cs }
 where
   filterElt xe@(XML.Elem el) 
     | pre el    = Just xe
     | otherwise = Nothing
   filterElt xe  = Just xe

addChild :: XML.Element -> XML.Element -> XML.Element
addChild a b = b { elContent = XML.Elem a : elContent b }

