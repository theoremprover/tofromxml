module Text.XML.ToFromXML.Backend.Parsec where

import Text.Parsec.Prim
import Text.Parsec.Combinator

type XMLParsecM a = ParsecT [SAXEvent String String] () IO a

parseSAXStream :: 
parseSAXStream saxstream = 