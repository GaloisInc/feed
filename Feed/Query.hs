module Feed.Query where

import Feed.Types as Feed

import RSS.Syntax  as RSS
import Atom.Feed   as Atom
import RSS1.Syntax as RSS1

import DublinCore.Types


feedItems :: Feed.Feed -> [Feed.Item]
feedItems fe = 
  case fe of
    AtomFeed f -> map Feed.AtomItem (Atom.feedEntries f)
    RSSFeed f  -> map Feed.RSSItem  (RSS.rssItems $ RSS.rssChannel f)
    RSS1Feed f -> map Feed.RSS1Item (RSS1.feedItems f)
    
itemCategories :: Feed.Item -> [String]
itemCategories it = 
  case it of
    Feed.AtomItem i -> map Atom.catTerm $ Atom.entryCategories i
    Feed.RSSItem i  -> map RSS.rssCategoryValue $ RSS.rssItemCategories i
    Feed.RSS1Item i -> concat $ getCats1 i
 where
    -- get RSS1 categories; either via DublinCore's subject (or taxonomy topics...not yet.)
   getCats1 i1 = 
     map (words.dcText) $ filter (\ dc -> dcElt dc == DC_Subject) $ RSS1.itemDC i1
