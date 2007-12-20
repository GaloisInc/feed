module Feed.Types where

import RSS.Syntax  as RSS
import Atom.Feed   as Atom
import RSS1.Syntax as RSS1
import Text.XML.Light as XML

-- Initially, we just wrap up the different feed formats in a sum type.
data Feed
 = AtomFeed Atom.Feed
 | RSSFeed  RSS.RSS
 | RSS1Feed RSS1.Feed
    -- if we're unable to correctly the well-formed XML as a feed,
    -- keep it as an untyped document.
 | XMLFeed  XML.Element
 
data Item
 = AtomItem Atom.Entry
 | RSSItem  RSS.RSSItem
 | RSS1Item RSS1.Item
 | XMLItem  XML.Element
 
