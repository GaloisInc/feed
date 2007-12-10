module Feed.Export where

import Feed.Types

import Atom.Feed.Export as Atom
import RSS.Export as RSS
import RSS1.Export as RSS1

import XML

xmlFeed :: Feed -> XML.Element
xmlFeed fe =
  case fe of
   AtomFeed f -> Atom.xmlFeed f
   RSSFeed  f -> RSS.xmlRSS f
   RSS1Feed f -> RSS1.xmlFeed f
   XMLFeed e  -> e -- that was easy!

