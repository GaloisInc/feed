module RSS1.Syntax where

import XML.Types as XML
import DublinCore.Types

type URIString   = String
type TitleString = String
type TimeString  = String
type TextString  = String

data Feed
 = Feed { feedVersion   :: String
        , feedChannel   :: Channel
	, feedImage     :: Maybe Image
	, feedItems     :: [Item]
	, feedTextInput :: Maybe TextInputInfo
	, feedTopics    :: [TaxonomyTopic]
	, feedOther     :: [XML.Element]
	, feedAttrs     :: [XML.Attr]
        }

data Channel
 = Channel
        { channelURI          :: URIString
	, channelTitle        :: TitleString
	, channelLink         :: URIString
	, channelDesc         :: TextString
           -- these are indirect RDF associations to elements declared
	   -- outside the channel element in the RDF \/ feed document.
	, channelImageURI     :: Maybe URIString
	, channelItemURIs     :: [URIString]
	, channelTextInputURI :: Maybe URIString
	, channelDC           :: [DCItem]
	, channelUpdatePeriod :: Maybe UpdatePeriod
	, channelUpdateFreq   :: Maybe Integer
	, channelUpdateBase   :: Maybe TimeString   -- format is yyyy-mm-ddThh:mm
	, channelContent      :: [ContentInfo]
	, channelTopics       :: [URIString]
	, channelOther        :: [XML.Element]
	, channelAttrs        :: [XML.Attr]
	}

data Image
 = Image
        { imageURI    :: URIString   -- the image resource, most likely.
	, imageTitle  :: TextString  -- the "alt"ernative text.
	, imageURL    :: URIString
	, imageLink   :: URIString   -- the href of the rendered img resource.
	, imageDC     :: [DCItem]
	, imageOther  :: [XML.Element]
	, imageAttrs  :: [XML.Attr]
	}

data Item
 = Item
        { itemURI     :: URIString
	, itemTitle   :: TextString
	, itemLink    :: URIString
	, itemDesc    :: Maybe TextString
	, itemDC      :: [DCItem]
	, itemTopics  :: [URIString]
	, itemContent :: [ContentInfo]
	, itemOther   :: [XML.Element]
	, itemAttrs   :: [XML.Attr]
	}

data TextInputInfo
 = TextInputInfo
        { textInputURI   :: URIString
	, textInputTitle :: TextString
	, textInputDesc  :: TextString
	, textInputName  :: TextString
	, textInputLink  :: URIString
	, textInputDC    :: [DCItem]
	, textInputOther :: [XML.Element]
	, textInputAttrs :: [XML.Attr]
	}

data TaxonomyTopic
 = TaxonomyTopic
        { taxonomyURI    :: URIString
	, taxonomyLink   :: URIString
	, taxonomyTitle  :: Maybe String
	, taxonomyDesc   :: Maybe String
	, taxonomyTopics :: [URIString]
	, taxonomyDC     :: [DCItem]
	, taxonomyOther  :: [XML.Element]
	}


data UpdatePeriod 
 = Update_Hourly
 | Update_Daily
 | Update_Weekly
 | Update_Monthly
 | Update_Yearly

data ContentInfo
 = ContentInfo
        { contentURI      :: Maybe URIString
	, contentFormat   :: Maybe URIString
	, contentEncoding :: Maybe URIString
	, contentValue    :: Maybe String -- should be: RDFValue
	}

