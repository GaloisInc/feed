module RSS.Syntax where

import XML

-- The Radio Userland version of RSS documents\/feeds.
-- (versions 0.9x, 2.x)
data RSS
 = RSS
     { rssVersion :: String
     , rssAttrs   :: [XML.Attr]
     , rssChannel :: RSSChannel
     , rssOther   :: [XML.Element]
     }

type URLString = String
type DateString = String

data RSSChannel
 = RSSChannel
     { rssTitle        :: String
     , rssLink         :: URLString
     , rssDescription  :: String
     , rssItems        :: [RSSItem]
     , rssLanguage     :: Maybe String
     , rssCopyright    :: Maybe String
     , rssEditor       :: Maybe String
     , rssWebMaster    :: Maybe String
     , rssPubDate      :: Maybe DateString  -- rfc 822 conforming.
     , rssLastUpdate   :: Maybe DateString
     , rssCategories   :: [RSSCategory]
     , rssGenerator    :: Maybe String
     , rssDocs         :: Maybe URLString
     , rssCloud        :: Maybe RSSCloud
     , rssTTL          :: Maybe Integer
     , rssImage        :: Maybe RSSImage
     , rssRating       :: Maybe String
     , rssTextInput    :: Maybe RSSTextInput
     , rssSkipHours    :: Maybe [Integer]
     , rssSkipDays     :: Maybe [String]
     , rssChannelOther :: [XML.Element]
     }

data RSSItem
 = RSSItem
     { rssItemTitle        :: Maybe String
     , rssItemLink         :: Maybe URLString
     , rssItemDescription  :: Maybe String     -- if not present, the title is. (per spec, at least.)
     , rssItemAuthor       :: Maybe String
     , rssItemCategories   :: [RSSCategory]
     , rssItemComments     :: Maybe URLString
     , rssItemEnclosure    :: Maybe RSSEnclosure
     , rssItemGuid         :: Maybe RSSGuid
     , rssItemPubDate      :: Maybe DateString
     , rssItemSource       :: Maybe RSSSource
     , rssItemAttrs        :: [XML.Attr]
     , rssItemOther        :: [XML.Element]
     }
     
data RSSSource
 = RSSSource
     { rssSourceURL    :: URLString
     , rssSourceAttrs  :: [XML.Attr]
     , rssSourceTitle  :: String
     }

data RSSEnclosure
 = RSSEnclosure
     { rssEnclosureURL     :: URLString
     , rssEnclosureLength  :: Integer
     , rssEnclosureType    :: String
     , rssEnclosureAttrs   :: [XML.Attr]
     }

data RSSCategory
 = RSSCategory
     { rssCategoryDomain   :: Maybe String
     , rssCategoryAttrs    :: [XML.Attr]
     , rssCategoryValue    :: String
     }

data RSSGuid
 = RSSGuid
     { rssGuidPermanentURL :: Maybe Bool
     , rssGuidAttrs        :: [XML.Attr]
     , rssGuidValue        :: String
     }


data RSSImage
 = RSSImage
     { rssImageURL     :: URLString -- the URL to the image resource.
     , rssImageTitle   :: String
     , rssImageLink    :: URLString -- URL that the image resource should be an href to.
     , rssImageWidth   :: Maybe Integer
     , rssImageHeight  :: Maybe Integer
     , rssImageDesc    :: Maybe String
     , rssImageOther   :: [XML.Element]
     }

data RSSCloud
 = RSSCloud
     { rssCloudDomain   :: Maybe String
     , rssCloudPort     :: Maybe String -- on purpose (i.e., not an int)
     , rssCloudPath     :: Maybe String
     , rssCloudRegister :: Maybe String
     , rssCloudProtocol :: Maybe String
     , rssCloudAttrs    :: [XML.Attr]
     }

data RSSTextInput
 = RSSTextInput
     { rssTextInputTitle :: String
     , rssTextInputDesc  :: String
     , rssTextInputName  :: String
     , rssTextInputLink  :: URLString
     , rssTextInputAttrs :: [XML.Attr]
     , rssTextInputOther :: [XML.Element]
     }


     
