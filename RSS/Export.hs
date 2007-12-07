module RSS.Export where

import XML
import RSS.Syntax

import Data.List
import Data.Maybe

qualNode :: String -> [XML.Content] -> XML.Element
qualNode n cs = 
  blank_element 
    { elName    = qualName n
    , elContent = children cs
    }

children :: [Content] -> Maybe [Content]
children [] = Nothing
children xs = Just xs


qualName :: String -> QName
qualName n = QName{qName=n,qURI=Nothing,qPrefix=Nothing}

---
xmlRSS :: RSS -> XML.Element
xmlRSS r = 
  (qualNode "rss" $ map Elem $
    (  [ xmlChannel (rssChannel r) ] 
    ++ rssOther r))
    { elAttribs = (Attr (qualName "version") (rssVersion r)):rssAttrs r }

xmlChannel :: RSSChannel -> XML.Element
xmlChannel ch = 
   (qualNode "channel" $ map Elem $
     ( [ xmlLeaf "title" (rssTitle ch) 
       , xmlLeaf "link"  (rssLink ch)
       , xmlLeaf "description" (rssDescription ch)
       ]
      ++ map xmlItem (rssItems ch)
      ++ mb (xmlLeaf "language")  (rssLanguage ch)
      ++ mb (xmlLeaf "copyright") (rssCopyright ch)
      ++ mb (xmlLeaf "managingEditor") (rssEditor ch)
      ++ mb (xmlLeaf "webMaster") (rssWebMaster ch)
      ++ mb (xmlLeaf "pubDate")   (rssPubDate ch)
      ++ mb (xmlLeaf "lastBuildDate") (rssLastUpdate ch)
      ++ map xmlCategory (rssCategories ch)
      ++ mb (xmlLeaf "generator") (rssGenerator ch)
      ++ mb (xmlLeaf "docs") (rssDocs ch)
      ++ mb xmlCloud (rssCloud ch)
      ++ mb ((xmlLeaf "ttl") . show) (rssTTL ch)
      ++ mb xmlImage (rssImage ch)
      ++ mb (xmlLeaf "rating") (rssRating ch)
      ++ mb xmlTextInput (rssTextInput ch)
      ++ mb xmlSkipHours (rssSkipHours ch)
      ++ mb xmlSkipDays  (rssSkipDays ch)
      ++ rssChannelOther ch))
      
xmlItem :: RSSItem -> XML.Element
xmlItem it = 
   (qualNode "item" $ map Elem $
     (  mb  (xmlLeaf "title") (rssItemTitle it) 
     ++ mb  (xmlLeaf "link")  (rssItemLink it)
     ++ mb  (xmlLeaf "description") (rssItemDescription it)
     ++ mb  (xmlLeaf "author") (rssItemAuthor it)
     ++ map xmlCategory (rssItemCategories it)
     ++ mb  (xmlLeaf "comments") (rssItemComments it)
     ++ mb  xmlEnclosure (rssItemEnclosure it)
     ++ mb  xmlGuid (rssItemGuid it)
     ++ mb  (xmlLeaf "pubDate") (rssItemPubDate it)
     ++ mb  xmlSource (rssItemSource it)
     ++ rssItemOther it))
      { elAttribs = rssItemAttrs it }

xmlSource :: RSSSource -> XML.Element
xmlSource s = 
   (xmlLeaf "source" (rssSourceTitle s))
     { elAttribs = (Attr (qualName "url") (rssSourceURL s)) : 
                   rssSourceAttrs s }

xmlEnclosure :: RSSEnclosure -> XML.Element
xmlEnclosure e = 
   (xmlLeaf "enclosure" "")
     { elAttribs =
        (Attr (qualName "url")    (rssEnclosureURL e)) : 
        (Attr (qualName "length") (show $ rssEnclosureLength e)) : 
        (Attr (qualName "type")   (rssEnclosureType e)) : 
	rssEnclosureAttrs e }

xmlCategory :: RSSCategory -> XML.Element
xmlCategory c = 
   (xmlLeaf "category" (rssCategoryValue c))
     { elAttribs =
        (fromMaybe id (fmap (\ n -> ((Attr (qualName "domain") n):))
	                    (rssCategoryDomain c))) $
	     (rssCategoryAttrs c) }

xmlGuid :: RSSGuid -> XML.Element
xmlGuid g = 
   (xmlLeaf "guid" (rssGuidValue g))
     { elAttribs =
        (fromMaybe id (fmap (\ n -> ((Attr (qualName "isPermaLink") (toBool n)):))
	                    (rssGuidPermanentURL g))) $
	     (rssGuidAttrs g) }
 where
  toBool False = "false"
  toBool _ = "true"

xmlImage :: RSSImage -> XML.Element
xmlImage im = 
   (qualNode "image" $ map Elem $
     ( [ xmlLeaf "url"   (rssImageURL im)
       , xmlLeaf "title" (rssImageTitle im)
       , xmlLeaf "link"  (rssImageLink im)
       ] 
       ++ mb ((xmlLeaf "width")  . show) (rssImageWidth im)
       ++ mb ((xmlLeaf "height") . show) (rssImageHeight im)
       ++ mb (xmlLeaf "description") (rssImageDesc im)
       ++ rssImageOther im))

xmlCloud :: RSSCloud -> XML.Element
xmlCloud cl = 
    (xmlLeaf "cloud" "")
     { elAttribs =
         (  mb (Attr (qualName "domain")) (rssCloudDomain cl)
	 ++ mb (Attr (qualName "port"))   (rssCloudPort cl)
	 ++ mb (Attr (qualName "path"))   (rssCloudPath cl)
	 ++ mb (Attr (qualName "register")) (rssCloudRegister cl)
	 ++ mb (Attr (qualName "protocol")) (rssCloudProtocol cl)
	 ++ rssCloudAttrs cl) }

xmlTextInput :: RSSTextInput -> XML.Element
xmlTextInput ti =
   (qualNode "textInput" $ map Elem $
     ( [ xmlLeaf "title" (rssTextInputTitle ti)
       , xmlLeaf "description"   (rssTextInputDesc ti)
       , xmlLeaf "name"  (rssTextInputName ti)
       , xmlLeaf "link"  (rssTextInputLink ti)
       ] ++ rssTextInputOther ti))
     { elAttribs = rssTextInputAttrs ti }

xmlSkipHours :: [Integer] -> XML.Element
xmlSkipHours hs = 
  (qualNode "skipHours" $ map Elem $
    (map (\ n -> xmlLeaf "hour" (show n)) hs))

xmlSkipDays :: [String] -> XML.Element
xmlSkipDays hs = 
  (qualNode "skipDayss" $ map Elem $
    (map (\ n -> xmlLeaf "day" n) hs))

--

xmlAttr :: String -> String -> XML.Attr
xmlAttr k v = Attr (qualName k) v

xmlLeaf :: String -> String -> XML.Element
xmlLeaf tg txt = 
 blank_element{ elName = qualName tg
 	      , elContent = Just [ Text blank_cdata { cdData = txt } ]
	      }

---
mb :: (a -> b) -> Maybe a -> [b]
mb _ Nothing = []
mb f (Just x) = [f x]

