module RSS.Syntax where

import Text.XML.Light as XML

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

-- default constructors:
nullRSS :: String -> URLString -> RSS
nullRSS title link = 
  RSS 
    { rssVersion = "2.0"
    , rssAttrs   = []
    , rssChannel = nullChannel title link
    , rssOther   = []
    }

nullChannel :: String -> URLString -> RSSChannel
nullChannel title link = 
  RSSChannel
     { rssTitle        = title
     , rssLink         = link
     , rssDescription  = title
     , rssItems        = []
     , rssLanguage     = Nothing
     , rssCopyright    = Nothing
     , rssEditor       = Nothing
     , rssWebMaster    = Nothing
     , rssPubDate      = Nothing
     , rssLastUpdate   = Nothing
     , rssCategories   = []
     , rssGenerator    = Nothing
     , rssDocs         = Nothing
     , rssCloud        = Nothing
     , rssTTL          = Nothing
     , rssImage        = Nothing
     , rssRating       = Nothing
     , rssTextInput    = Nothing
     , rssSkipHours    = Nothing
     , rssSkipDays     = Nothing
     , rssChannelOther = []
     }

nullItem :: String -> RSSItem
nullItem title = 
   RSSItem
     { rssItemTitle        = Just title
     , rssItemLink         = Nothing
     , rssItemDescription  = Nothing
     , rssItemAuthor       = Nothing
     , rssItemCategories   = []
     , rssItemComments     = Nothing
     , rssItemEnclosure    = Nothing
     , rssItemGuid         = Nothing
     , rssItemPubDate      = Nothing
     , rssItemSource       = Nothing
     , rssItemAttrs        = []
     , rssItemOther        = []
     }

nullSource :: URLString -> String -> RSSSource
nullSource url title = 
  RSSSource
     { rssSourceURL    = url
     , rssSourceAttrs  = []
     , rssSourceTitle  = title
     }

nullEnclosure :: URLString -> Integer -> String -> RSSEnclosure
nullEnclosure url len ty = 
  RSSEnclosure
     { rssEnclosureURL     = url
     , rssEnclosureLength  = len
     , rssEnclosureType    = ty
     , rssEnclosureAttrs   = []
     }

newCategory :: String -> RSSCategory
newCategory nm = 
  RSSCategory
     { rssCategoryDomain   = Nothing
     , rssCategoryAttrs    = []
     , rssCategoryValue    = nm
     }

nullGuid :: String -> RSSGuid
nullGuid v = 
  RSSGuid
     { rssGuidPermanentURL = Nothing
     , rssGuidAttrs        = []
     , rssGuidValue        = v
     }

nullPermaGuid :: String -> RSSGuid
nullPermaGuid v = (nullGuid v){rssGuidPermanentURL=Just True}

nullImage :: URLString -> String -> URLString -> RSSImage
nullImage url title link = 
  RSSImage
     { rssImageURL     = url
     , rssImageTitle   = title
     , rssImageLink    = link
     , rssImageWidth   = Nothing
     , rssImageHeight  = Nothing
     , rssImageDesc    = Nothing
     , rssImageOther   = []
     }

nullCloud :: RSSCloud
nullCloud = 
  RSSCloud
     { rssCloudDomain   = Nothing
     , rssCloudPort     = Nothing
     , rssCloudPath     = Nothing
     , rssCloudRegister = Nothing
     , rssCloudProtocol = Nothing
     , rssCloudAttrs    = []
     }

nullTextInput :: String -> String -> URLString -> RSSTextInput
nullTextInput title nm link = 
  RSSTextInput
     { rssTextInputTitle = title
     , rssTextInputDesc  = title
     , rssTextInputName  = nm
     , rssTextInputLink  = link
     , rssTextInputAttrs = []
     , rssTextInputOther = []
     }

