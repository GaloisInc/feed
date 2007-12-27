module Feed.Types where

import RSS.Syntax  as RSS
import Atom.Feed   as Atom
import RSS1.Syntax as RSS1
import Text.XML.Light as XML

-- | The abstract type of feed documents. The internal representation
-- is as whatever feed variant type the document was either imported or
-- has now been translated to.
data Feed
 = AtomFeed Atom.Feed
 | RSSFeed  RSS.RSS
 | RSS1Feed RSS1.Feed
    -- if we're unable to correctly the well-formed XML as a feed,
    -- keep it as an untyped document.
 | XMLFeed  XML.Element
 
-- | The abstract type of feed items. Like the 'Feed' type, the
-- representation of a value is as one of the different RSS item\/entry
-- variants.
data Item
 = AtomItem Atom.Entry
 | RSSItem  RSS.RSSItem
 | RSS1Item RSS1.Item
 | XMLItem  XML.Element
 
-- | The kinds of feed documents supported.
data FeedKind
 = AtomKind
 | RSSKind (Maybe String)  -- Nothing => default version (2.0)
 | RDFKind (Maybe String)  -- Nothing => default version (1.0)
   deriving ( Eq )

