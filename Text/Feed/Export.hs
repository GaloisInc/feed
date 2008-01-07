--------------------------------------------------------------------
-- |
-- Module    : Text.Feed.Export
-- Copyright : (c) Galois, Inc. 2008
-- License   : BSD3
--
-- Maintainer: Don Stewart <dons@galois.com>
-- Stability : provisional
-- Portability:
--
--------------------------------------------------------------------


module Text.Feed.Export where

import Text.Feed.Types

import Text.Atom.Feed.Export as Atom
import Text.RSS.Export as RSS
import Text.RSS1.Export as RSS1

import Text.XML.Light as XML

xmlFeed :: Feed -> XML.Element
xmlFeed fe =
  case fe of
   AtomFeed f -> Atom.xmlFeed f
   RSSFeed  f -> RSS.xmlRSS f
   RSS1Feed f -> RSS1.xmlFeed f
   XMLFeed e  -> e -- that was easy!

