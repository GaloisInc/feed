--------------------------------------------------------------------
-- |
-- Module    : Text.Feed.Constructor
-- Copyright : (c) Galois, Inc. 2008
-- License   : BSD3
--
-- Maintainer: Don Stewart <dons@galois.com>
-- Stability : provisional
-- Portability:
--
--------------------------------------------------------------------

module Text.Feed.Constructor
       ( FeedKind(..)
       , newFeed         -- :: FeedKind -> Feed
       , getFeedKind     -- :: Feed     -> FeedKind
       , addItem         -- :: Item -> Feed -> Feed
       , withFeedTitle   -- :: String -> Feed -> Feed
       , withFeedHome    -- :: URLString -> Feed -> Feed
       , withFeedHTML    -- :: URLString -> Feed -> Feed
       

       , newItem              -- :: FeedKind   -> Item
       , getItemKind          -- :: Item       -> FeedKind
       , atomEntryToItem      -- :: Atom.Entry -> Item
       , rssItemToItem        -- :: RSS.Item   -> Item
       , rdfItemToItem        -- :: RSS1.Item  -> Item

       , withItemTitle        -- :: String     -> Item -> Item
       , withItemLink         -- :: URLString  -> Item -> Item
       , withItemPubDate      -- :: DateString -> Item -> Item
       , withItemDate         -- :: DateString -> Item -> Item
       , withItemAuthor       -- :: String     -> Item -> Item
       , withItemCommentLink  -- :: String     -> Item -> Item
       , withItemEnclosure    -- :: String  -> Maybe String -> Integer -> Item -> Item
       , withItemFeedLink     -- :: String -> String -> Item -> Item
       , withItemId           -- :: Bool -> String -> Item -> Item
       , withItemCategories   -- :: [(String, Maybe String)] -> Item -> Item
       , withItemDescription  -- :: String     -> Item -> Item
       , withItemRights       -- :: String     -> Item -> Item
       ) where

import Text.Feed.Types      as Feed.Types

import Text.Atom.Feed       as Atom
import Text.RSS.Syntax      as RSS
import Text.RSS1.Syntax     as RSS1
import Text.DublinCore.Types
import Text.XML.Light as XML

import Data.Maybe ( fromMaybe, mapMaybe )
import Data.Char  ( toLower )

-- | Construct an empty feed document, intending to output it in 
-- the 'fk' feed format.
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
    AtomKind  -> Feed.Types.AtomItem $
      Atom.nullEntry "entry-id-not-filled-in"
                     (TextString "dummy-entry-title")
                     "dummy-and-bogus-entry-update-date"
    RSSKind{} -> Feed.Types.RSSItem $
      RSS.nullItem "dummy-rss-item-title"
    RDFKind{} -> Feed.Types.RSS1Item $
      RSS1.nullItem "dummy-item-uri"
                    "dummy-item-title"
                    "dummy-item-link"

getItemKind :: Feed.Types.Item -> FeedKind
getItemKind f = 
  case f of
    Feed.Types.AtomItem{} -> AtomKind
    Feed.Types.RSSItem{}  -> RSSKind (Just "2.0") -- good guess..
    Feed.Types.RSS1Item{} -> RDFKind (Just "1.0")
    Feed.Types.XMLItem{}  -> RSSKind (Just "2.0")

withFeedTitle :: String -> Feed.Types.Feed -> Feed.Types.Feed
withFeedTitle tit fe = 
  case fe of
   Feed.Types.AtomFeed f -> Feed.Types.AtomFeed f{feedTitle=TextString tit}
   Feed.Types.RSSFeed  f -> Feed.Types.RSSFeed  f{rssChannel=(rssChannel f){rssTitle=tit}}
   Feed.Types.RSS1Feed f -> Feed.Types.RSS1Feed f{feedChannel=(feedChannel f){channelTitle=tit}}
   Feed.Types.XMLFeed  f -> Feed.Types.XMLFeed $
      mapMaybeChildren (\ e -> 
        if (elName e == unqual "channel")
	 then Just (mapMaybeChildren (\ e2 -> 
	                if (elName e2 == unqual "title")
			 then Just (node (unqual "title",tit))
			 else Nothing) e)
	 else Nothing) f

withFeedHome :: URLString -> Feed.Types.Feed -> Feed.Types.Feed
withFeedHome url fe = 
  case fe of
   Feed.Types.AtomFeed f -> Feed.Types.AtomFeed f{feedLinks=newSelf:Atom.feedLinks f}
      -- ToDo: fix, the <link> element is for the HTML home of the channel, not the
      -- location of the feed itself. Struggling to find if there is a common way
      -- to represent this outside of RSS 2.0 standard elements..
   Feed.Types.RSSFeed  f -> Feed.Types.RSSFeed  f{rssChannel=(rssChannel f){rssLink=url}}
   Feed.Types.RSS1Feed f -> Feed.Types.RSS1Feed f{feedChannel=(feedChannel f){channelURI=url}}
   Feed.Types.XMLFeed  f -> Feed.Types.XMLFeed $
      mapMaybeChildren (\ e -> 
        if (elName e == unqual "channel")
	 then Just (mapMaybeChildren (\ e2 -> 
	                if (elName e2 == unqual "link")
			 then Just (node (unqual "link",url))
			 else Nothing) e)
	 else Nothing) f
 where
  newSelf = (nullLink url){ linkRel=Just (Left "self")
                          , linkType=Just "application/atom+xml" 
			  }

withFeedHTML :: URLString -> Feed.Types.Feed -> Feed.Types.Feed
withFeedHTML url fe = 
  case fe of
   Feed.Types.AtomFeed f -> Feed.Types.AtomFeed f{feedLinks=newAlt:Atom.feedLinks f}
   Feed.Types.RSSFeed  f -> Feed.Types.RSSFeed  f{rssChannel=(rssChannel f){rssLink=url}}
   Feed.Types.RSS1Feed f -> Feed.Types.RSS1Feed f{feedChannel=(feedChannel f){channelLink=url}}
   Feed.Types.XMLFeed  f -> Feed.Types.XMLFeed $
      mapMaybeChildren (\ e -> 
        if (elName e == unqual "channel")
	 then Just (mapMaybeChildren (\ e2 -> 
	                if (elName e2 == unqual "link")
			 then Just (node (unqual "link",url))
			 else Nothing) e)
	 else Nothing) f
 where
  newAlt = (nullLink url){ linkRel=Just (Left "alternate")
                          , linkType=Just "text/html" 
			  }


-- Item constructors (all the way to the end):

atomEntryToItem :: Atom.Entry -> Feed.Types.Item
atomEntryToItem e = Feed.Types.AtomItem e

rssItemToItem :: RSS.RSSItem -> Feed.Types.Item
rssItemToItem i = Feed.Types.RSSItem i

rdfItemToItem :: RSS1.Item -> Feed.Types.Item
rdfItemToItem i = Feed.Types.RSS1Item i

-- | 'withItemDate updDate' associates the last-updated date, 'updDate',
-- with a feed item. If the RSS variant doesn't support the notion of
-- last-updated, 'updDate' is set equal to the creation time.
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
        addChild (node (unqual "pubDate", dt)) $
            filterChildren (\ e -> elName e /= unqual "pubDate")
                           i
 where
  isDate dc  = dcElt dc == DC_Date

withItemPubDate :: DateString -> Feed.Types.Item -> Feed.Types.Item
withItemPubDate dt fi = withItemDate dt fi

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
        addChild (node (unqual "title",tit)) $
            filterChildren (\ e -> elName e /= unqual "title")
                           i

-- | 'withItemAuthor auStr' associates new author info
-- with a feed item.
withItemAuthor :: String -> Feed.Types.Item -> Feed.Types.Item
withItemAuthor au fi = 
  case fi of
    Feed.Types.AtomItem e ->
      Feed.Types.AtomItem e{Atom.entryAuthors=[nullPerson{personName=au,personURI=Just au}]}
    Feed.Types.RSSItem i  ->
      Feed.Types.RSSItem  i{RSS.rssItemAuthor=Just au}
    Feed.Types.RSS1Item i ->
      case break isAuthor $ RSS1.itemDC i of
       (as,(dci:bs)) -> Feed.Types.RSS1Item i{RSS1.itemDC=as++dci{dcText=au}:bs}
       (_,[]) -> Feed.Types.RSS1Item i{RSS1.itemDC=DCItem{dcElt=DC_Creator,dcText=au}:RSS1.itemDC i}
    Feed.Types.XMLItem i  ->
      Feed.Types.XMLItem $
        addChild (node (unqual "author",au)) $
            filterChildren (\ e -> elName e /= unqual "author")
                           i
 where
  isAuthor dc  = dcElt dc == DC_Creator

-- | 'withItemFeedLink name myFeed' associates the parent feed URL 'myFeed'
-- with a feed item. It is labelled as 'name'.
withItemFeedLink :: String -> String -> Feed.Types.Item -> Feed.Types.Item
withItemFeedLink tit url fi = 
  case fi of
    Feed.Types.AtomItem e ->
      Feed.Types.AtomItem e{Atom.entrySource=Just Atom.nullSource{sourceId=Just url,sourceTitle=Just (TextString tit)}}
    Feed.Types.RSSItem i  ->
      Feed.Types.RSSItem  i{RSS.rssItemSource=Just (RSS.nullSource url tit)}
    Feed.Types.RSS1Item i ->
      Feed.Types.RSS1Item  i{RSS1.itemTitle=tit}
    Feed.Types.XMLItem i  ->
      Feed.Types.XMLItem $
        addChild (node (unqual "source",Attr (unqual "url") url,tit)) $
            filterChildren (\ e -> elName e /= unqual "source")
                           i



-- | 'withItemCommentLink url' sets the URL reference to the comment page to 'url'.
withItemCommentLink :: String -> Feed.Types.Item -> Feed.Types.Item
withItemCommentLink url fi = 
  case fi of
    Feed.Types.AtomItem e ->
      Feed.Types.AtomItem e{Atom.entryLinks=((nullLink url){linkRel=Just (Left "replies")}):Atom.entryLinks e}
    Feed.Types.RSSItem i  ->
      Feed.Types.RSSItem  i{RSS.rssItemComments=Just url}
    Feed.Types.RSS1Item i ->
      case break isRel $ RSS1.itemDC i of
       (as,(dci:bs)) -> Feed.Types.RSS1Item i{RSS1.itemDC=as++dci{dcText=url}:bs}
       (_,[]) -> Feed.Types.RSS1Item i{RSS1.itemDC=DCItem{dcElt=DC_Relation,dcText=url}:RSS1.itemDC i}
    Feed.Types.XMLItem i  ->
      Feed.Types.XMLItem $
        addChild (node (unqual "comments",url)) $
            filterChildren (\ e -> elName e /= unqual "comments")
                           i
 where
  isRel dc  = dcElt dc == DC_Relation

-- | 'withItemEnclosure url mbTy len' sets the URL reference to the comment page to 'url'.
withItemEnclosure :: String -> Maybe String -> Integer -> Feed.Types.Item -> Feed.Types.Item
withItemEnclosure url ty len fi = 
  case fi of
    Feed.Types.AtomItem e -> Feed.Types.AtomItem 
       e{Atom.entryLinks=((nullLink url){linkRel=Just (Left "enclosure")
                                        ,linkType=ty
                                        ,linkLength=Just (show len)
                                        }):Atom.entryLinks e}
    Feed.Types.RSSItem i  ->
      Feed.Types.RSSItem  i{RSS.rssItemEnclosure=Just (nullEnclosure url len (fromMaybe "text/html" ty))}
    Feed.Types.RSS1Item i -> Feed.Types.RSS1Item 
          i{RSS1.itemContent=nullContentInfo{ contentURI=Just url
                                            , contentFormat=ty
                                            }:RSS1.itemContent i}
    Feed.Types.XMLItem i  ->
      Feed.Types.XMLItem $
        addChild ((node (unqual "enclosure",url))
          {elAttribs= [ Attr (unqual "length") "0"
                      , Attr (unqual "type") (fromMaybe "text/html" ty)
                      ]}) $
            filterChildren (\ e -> elName e /= unqual "enclosure")
                           i


-- | 'withItemId isURL id' associates new unique identifier with a feed item.
-- If 'isURL' is 'True', then the id is assumed to point to a valid web resource.
withItemId :: Bool -> String -> Feed.Types.Item -> Feed.Types.Item
withItemId isURL idS fi = 
  case fi of
    Feed.Types.AtomItem e ->
      Feed.Types.AtomItem e{Atom.entryId=idS}
    Feed.Types.RSSItem i  ->
      Feed.Types.RSSItem  i{RSS.rssItemGuid=Just (nullGuid idS){rssGuidPermanentURL=Just isURL}}
    Feed.Types.RSS1Item i ->
      case break isId $ RSS1.itemDC i of
       (as,(dci:bs)) -> Feed.Types.RSS1Item i{RSS1.itemDC=as++dci{dcText=idS}:bs}
       (_,[]) -> Feed.Types.RSS1Item i{RSS1.itemDC=DCItem{dcElt=DC_Identifier,dcText=idS}:RSS1.itemDC i}
    Feed.Types.XMLItem i  ->
      Feed.Types.XMLItem $
        addChild (node (unqual "guid",Attr (unqual "isPermaLink") (showBool isURL),idS)) $
            filterChildren (\ e -> elName e /= unqual "guid")
                           i
 where
  showBool x  = map toLower (show x)
  isId dc     = dcElt dc == DC_Identifier

-- | 'withItemDescription desc' associates a new descriptive string (aka summary)
-- with a feed item.
withItemDescription :: String -> Feed.Types.Item -> Feed.Types.Item
withItemDescription desc fi = 
  case fi of
    Feed.Types.AtomItem e ->
      Feed.Types.AtomItem e{Atom.entrySummary=Just (TextString desc)}
    Feed.Types.RSSItem i  ->
      Feed.Types.RSSItem  i{RSS.rssItemDescription=Just desc}
    Feed.Types.RSS1Item i ->
      Feed.Types.RSS1Item  i{RSS1.itemDesc=Just desc}
    Feed.Types.XMLItem i  ->
      Feed.Types.XMLItem $
        addChild (node (unqual "description",desc)) $
            filterChildren (\ e -> elName e /= unqual "description")
                           i

-- | 'withItemRights rightStr' associates the rights information 'rightStr'
-- with a feed item.
withItemRights :: String -> Feed.Types.Item -> Feed.Types.Item
withItemRights desc fi = 
  case fi of
    Feed.Types.AtomItem e ->
      Feed.Types.AtomItem e{Atom.entryRights=Just (TextString desc)}
     -- Note: per-item copyright information isn't supported by RSS2.0 (and earlier editions),
     -- you can only attach this at the feed/channel level. So, there's not much we can do
     -- except dropping the information on the floor here. (Rolling our own attribute or
     -- extension element is an option, but would prefer if someone else had started that
     -- effort already.
    Feed.Types.RSSItem{}  -> fi
    Feed.Types.RSS1Item i ->
      case break ((==DC_Rights).dcElt) $ RSS1.itemDC i of
       (as,(dci:bs)) -> Feed.Types.RSS1Item i{RSS1.itemDC=as++dci{dcText=desc}:bs}
       (_,[]) -> Feed.Types.RSS1Item i{RSS1.itemDC=DCItem{dcElt=DC_Rights,dcText=desc}:RSS1.itemDC i}
     -- Since we're so far assuming that a shallow XML rep. of an item
     -- is of RSS2.0 ilk, pinning on the rights info is hard (see above.)
    Feed.Types.XMLItem{}  -> fi

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
        addChild (node (unqual "link", url)) $
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
    
withItemCategories :: [(String,Maybe String)]
                   -> Feed.Types.Item
                   -> Feed.Types.Item
withItemCategories cats fi = 
  case fi of
    Feed.Types.AtomItem e -> Feed.Types.AtomItem 
        e{ Atom.entryCategories =
                map ( \ (t,mb) -> (Atom.newCategory t){Atom.catScheme=mb})
                    cats ++ entryCategories e}
    Feed.Types.RSSItem i  -> Feed.Types.RSSItem 
        i{RSS.rssItemCategories=
              map (\ (t,mb) -> (RSS.newCategory t){RSS.rssCategoryDomain=mb})
                  cats ++ rssItemCategories i}
    Feed.Types.RSS1Item i -> Feed.Types.RSS1Item 
         i{RSS1.itemDC=
                map (\ (t,_) -> DCItem{dcElt=DC_Subject,dcText=t})
                    cats ++ RSS1.itemDC i}
    Feed.Types.XMLItem i  -> Feed.Types.XMLItem $
         foldr (\ (t,mb) acc -> 
                  addChild (node ( unqual "category"
                                 , (fromMaybe (\x -> [x])
                                             (fmap (\v -> (\ x -> [Attr (unqual "domain") v,x])) mb) $
                                             (Attr (unqual "term") t))
                                 )) acc)
               i
               cats

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

mapMaybeChildren :: (XML.Element -> Maybe XML.Element)
                 -> XML.Element
		 -> XML.Element
mapMaybeChildren f e = 
  case elContent e of
    [] -> e
    cs -> e { elContent = map procElt cs }
 where
   procElt xe@(XML.Elem el) =
     case f el of
       Nothing  -> xe
       Just el1 -> XML.Elem el1
   procElt xe = xe
