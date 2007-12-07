module Feed.Types where

import RSS.Syntax  as RSS
import Atom.Feed   as Atom
import RSS1.Syntax as RSS1

-- Initially, we just wrap up the different feed formats in a sum type.
data Feed
 = AtomFeed Atom.Feed
 | RSSFeed  RSS.RSS
 | RSS1Feed RSS1.Feed
 
data Item
 = AtomItem Atom.Entry
 | RSSItem  RSS.RSSItem
 | RSS1Item RSS1.Item
 
