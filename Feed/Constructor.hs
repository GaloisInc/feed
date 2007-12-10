module Feed.Constructor where

import Feed.Types

import Atom.Feed as Atom
import RSS.Syntax as RSS
import RSS1.Syntax as RSS1

import Data.Maybe ( fromMaybe )

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
