module Feed.Query 
       ( Feed.Query.feedItems -- :: Feed.Feed -> [Feed.Item]
       , getCategories        -- :: Feed.Item -> [String]
       , getPublishDate       -- :: Feed.Item -> Maybe String
       , getLink              -- :: Feed.Item -> Maybe String
       , getTitle	      -- :: Feed.Item -> Maybe String
       ) where

import Feed.Types as Feed

import RSS.Syntax  as RSS
import Atom.Feed   as Atom
import RSS1.Syntax as RSS1
import XML

import DublinCore.Types

import Data.List

feedItems :: Feed.Feed -> [Feed.Item]
feedItems fe = 
  case fe of
    AtomFeed f -> map Feed.AtomItem (Atom.feedEntries f)
    RSSFeed f  -> map Feed.RSSItem  (RSS.rssItems $ RSS.rssChannel f)
    RSS1Feed f -> map Feed.RSS1Item (RSS1.feedItems f)
     -- ToDo: look for 'entry' elements if 'items' are missing..
    XMLFeed f  -> map Feed.XMLItem $ XML.findElements (XML.unqual "item") f
    
getCategories :: Feed.Item -> [String]
getCategories it = 
  case it of
    Feed.AtomItem i -> map Atom.catTerm $ Atom.entryCategories i
    Feed.RSSItem i  -> map RSS.rssCategoryValue $ RSS.rssItemCategories i
    Feed.RSS1Item i -> concat $ getCats1 i
    Feed.XMLItem i  -> map XML.strContent $ XML.findElements (XML.unqual "category") i
 where
    -- get RSS1 categories; either via DublinCore's subject (or taxonomy topics...not yet.)
   getCats1 i1 = 
     map (words.dcText) $ filter (\ dc -> dcElt dc == DC_Subject) $ RSS1.itemDC i1

getLink :: Feed.Item -> Maybe String
getLink it = 
  case it of
    Feed.AtomItem i -> 
       -- look up the 'alternate' HTML link relation on the entry:
       case filter isSelf $ Atom.entryLinks i of
         (l:_) -> Just (Atom.linkHref l)
	 _ -> Nothing
    Feed.RSSItem i  -> RSS.rssItemLink i
    Feed.RSS1Item i -> Just (RSS1.itemLink i)
    Feed.XMLItem i  -> fmap (\ ei -> XML.strContent ei) $ findElement (unqual "link") i
 where
  isSelf lr = Atom.linkRel lr == Just (Left "alternate") && isHTMLType (linkType lr)
  
  isHTMLType (Just str) = "lmth" `isPrefixOf` (reverse str)
  isHTMLType _ = False
  
getTitle :: Feed.Item -> Maybe String
getTitle it = 
  case it of
    Feed.AtomItem i -> Just (toStr $ Atom.entryTitle i)
    Feed.RSSItem i  -> RSS.rssItemTitle i
    Feed.RSS1Item i -> Just (RSS1.itemTitle i)
    Feed.XMLItem e  -> fmap XML.strContent $ findElement (unqual "title") e
 where
   toStr (Atom.TextString s) = s
   toStr (Atom.HTMLString s) = s
   toStr (Atom.XHTMLString x) = XML.strContent x

getPublishDate :: Feed.Item -> Maybe String
getPublishDate it = 
  case it of
    Feed.AtomItem i -> Atom.entryPublished i
    Feed.RSSItem i  -> RSS.rssItemPubDate i
    Feed.RSS1Item i -> 
      case filter isDate $ RSS1.itemDC i of
       (dci:_) -> Just (dcText dci)
       _ -> Nothing
      -- ToDo: look for it in Atom \/ RSS1 like-content as well if no 'pubDate' element.
    Feed.XMLItem e  -> fmap XML.strContent $ findElement (unqual "pubDate") e
 where
  isDate dc  = dcElt dc == DC_Date
