module Atom.Feed where

import qualified Text.XML.Light as XML

-- NOTE: In the future we may want to have more structured
-- types for these.
type URI        = String
type NCName     = String
type Date       = String
type MediaType  = String

data Feed
 = Feed
      { feedId           :: String
      , feedTitle        :: TextContent
      , feedUpdated      :: Date
      , feedAuthors      :: [Person]
      , feedCategories   :: [Category]
      , feedContributors :: [Person]
      , feedGenerator    :: Maybe Generator
      , feedIcon         :: Maybe URI
      , feedLinks        :: [Link]
      , feedLogo         :: Maybe URI
      , feedRights       :: Maybe TextContent
      , feedSubtitle     :: Maybe TextContent
      , feedEntries      :: [Entry]
      , feedAttr         :: [XML.Attr]
      , feedOther        :: [XML.Element]
      }

nullFeed :: String -> TextContent -> Date -> Feed
nullFeed i t u = Feed
      { feedId           = i
      , feedTitle        = t
      , feedUpdated      = u
      , feedAuthors      = []
      , feedCategories   = []
      , feedContributors = []
      , feedGenerator    = Nothing
      , feedIcon         = Nothing
      , feedLinks        = []
      , feedLogo         = Nothing
      , feedRights       = Nothing
      , feedSubtitle     = Nothing
      , feedEntries      = []
      , feedAttr         = []
      , feedOther        = []
      }

data Entry
 = Entry
      { entryId          :: String
      , entryTitle       :: TextContent
      , entryUpdated     :: Date
      , entryAuthors     :: [Person]
      , entryCategories  :: [Category]
      , entryContent     :: Maybe EntryContent
      , entryContributor :: [Person]
      , entryLinks       :: [Link]
      , entryPublished   :: Maybe Date
      , entryRights      :: Maybe TextContent
      , entrySource      :: Maybe Source
      , entrySummary     :: Maybe TextContent
      , entryInReplyTo   :: Maybe InReplyTo
      , entryInReplyTotal  :: Maybe InReplyTotal
      , entryOther       :: [XML.Element]
      }

nullEntry :: String -> TextContent -> Date -> Entry
nullEntry i t u = Entry
      { entryId          = i
      , entryTitle       = t
      , entryUpdated     = u
      , entryAuthors     = []
      , entryCategories  = []
      , entryContent     = Nothing
      , entryContributor = []
      , entryLinks       = []
      , entryPublished   = Nothing
      , entryRights      = Nothing
      , entrySource      = Nothing
      , entrySummary     = Nothing
      , entryInReplyTo   = Nothing
      , entryInReplyTotal= Nothing
      , entryOther       = []
      }

data EntryContent
 = TextContent   String
 | HTMLContent   String
 | XHTMLContent  XML.Element
 | MixedContent  (Maybe String) [XML.Content]
 | ExternalContent (Maybe MediaType) URI

data Category
 = Category
       { catTerm   :: String         -- ^ the tag\/term of the category.
       , catScheme :: Maybe URI      -- ^ optional URL for identifying the categorization scheme.
       , catLabel  :: Maybe String   -- ^ human-readable label of the category
       , catOther  :: [XML.Element]  -- ^ unknown elements, for extensibility.
       }

newCategory :: String -> Category
newCategory t = Category
  { catTerm   = t
  , catScheme = Nothing
  , catLabel  = Just t
  , catOther  = []
  }

data Generator
 = Generator
       { genURI     :: Maybe URI
       , genVersion :: Maybe String
       , genText    :: String
       }

data Link
 = Link
      { linkHref     :: URI
         -- ToDo: make the switch over to using the Atom.Feed.Link relation type.
      , linkRel      :: Maybe (Either NCName URI)
      , linkType     :: Maybe MediaType
      , linkHrefLang :: Maybe String
      , linkTitle    :: Maybe String
      , linkLength   :: Maybe String
      , linkOther    :: [XML.Element]
      }

nullLink :: URI -> Link
nullLink uri = Link
  { linkHref      = uri
  , linkRel       = Nothing
  , linkType      = Nothing
  , linkHrefLang  = Nothing
  , linkTitle     = Nothing
  , linkLength    = Nothing
  , linkOther     = []
  }

data TextContent
 = TextString  String
 | HTMLString  String
 | XHTMLString XML.Element

data Source
 = Source
      { sourceAuthors     :: [Person]
      , sourceCategories  :: [Category]
      , sourceGenerator   :: Maybe Generator
      , sourceIcon        :: Maybe URI
      , sourceId          :: Maybe String
      , sourceLinks       :: [Link]
      , sourceLogo        :: Maybe URI
      , sourceRights      :: Maybe TextContent
      , sourceSubtitle    :: Maybe TextContent
      , sourceTitle       :: Maybe TextContent
      , sourceUpdated     :: Maybe Date
      , sourceOther       :: [XML.Element]
      }

data Person
 = Person
     { personName  :: String
     , personURI   :: Maybe URI
     , personEmail :: Maybe String
     , personOther :: [XML.Element]
     }

nullPerson :: Person
nullPerson = Person
  { personName  = ""
  , personURI   = Nothing
  , personEmail = Nothing
  , personOther = []
  }

data InReplyTo
 = InReplyTo
     { replyToRef     :: URI
     , replyToHRef    :: Maybe URI
     , replyToType    :: Maybe MediaType
     , replyToSource  :: Maybe URI
     , replyToOther   :: [XML.Attr]
     , replyToContent :: [XML.Content]
     }

data InReplyTotal
 = InReplyTotal
     { replyToTotal      :: Integer -- non-negative :)
     , replyToTotalOther :: [XML.Attr]
     }
