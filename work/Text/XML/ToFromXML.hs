{-# LANGUAGE DeriveGeneric,DefaultSignatures,UndecidableInstances,ScopedTypeVariables,
	OverlappingInstances,TypeOperators,FlexibleContexts,FlexibleInstances #-}

module Text.XML.ToFromXML (
	FromToXML,GFromToXML,
	toXML,fromXML,fromXMLEither,
	readFileFromXML,writeFileToXML,
	module GHC.Generics
	) where

import GHC.Generics

import qualified Data.ByteString as BS

import Text.XML.Expat.Pickle
import Text.XML.Expat.Tree
import qualified Text.XML.Expat.Format as Format

import Text.Printf

import Data.Char
import Data.Word
import Data.Ratio
import Data.Int
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import Data.Complex
import Data.Array.IArray

-- | For the picklers, are using nodes with both tag and text being Strings.
type Pickler a = PU (ListOf (UNode String)) a

class GFromToXML f where
	gXMLPickler :: Pickler (f p)

instance (GFromToXML f1,GFromToXML f2) => GFromToXML (f1 :*: f2) where
	gXMLPickler = xpWrap (uncurry (:*:),\ (a :*: b) -> (a,b)) $
--		xpPair gXMLPickler gXMLPickler
		xpElemNodes "PRODUCT" $ xpPair
			(xpElemNodes "FIRST"  gXMLPickler)
			(xpElemNodes "SECOND" gXMLPickler) 

instance (GFromToXML f1,GFromToXML f2) => GFromToXML (f1 :+: f2) where
	gXMLPickler = xpAlt selfun [
		xpWrap (L1,\ (L1 x) -> x) gXMLPickler,
		xpWrap (R1,\ (R1 x) -> x) gXMLPickler ] where
		selfun (L1 _) = 0
		selfun (R1 _) = 1

instance (GFromToXML f,Datatype d) => GFromToXML (M1 D d f) where
	gXMLPickler = xpWrap (M1,unM1) gXMLPickler

{-|
The CONSTRUCTOR tag contains a name attribute with the constructor's name.
Strictly speaking it is unnecessary to encode the name as well, but improves readability
and might add to safety...
-}
instance (GFromToXML f,Constructor c) => GFromToXML (M1 C c f) where
	gXMLPickler = xpWrap (M1,unM1) $ xpElemWithNameAttr "CONSTRUCTOR" conname gXMLPickler where
		conname = conName (undefined :: M1 C c f p)

{-|
The SELECTOR tag contains a name attribute with the field's name.
Strictly speaking this tag is unnecessary for encoding, since records are represented already generically
by two-dimensional type products, but it improves readability.
-}
instance (GFromToXML f,Selector s) => GFromToXML (M1 S s f) where
	gXMLPickler = xpWrap (M1,unM1) $ xpElemWithNameAttr "SELECTOR" selname gXMLPickler where
		selname = selName (undefined :: M1 S s f p)

-- | A helper function injecting a "name" attribute with a given/fixed value.
xpElemWithNameAttr tag attrval pickler = xpWrap (snd,\b->((),b)) $
	xpElem tag (xpAttrFixed "name" attrval) pickler

{-
V1 is not an instance of GFromToXML, because we can't serialize data of an empty type since there
are no constructors. It shouldn't be necessary to serialize an empty type, so V1 should be rejected by the type checker.
-}

instance (GFromToXML f) => GFromToXML (M1 S NoSelector f) where
	gXMLPickler = xpWrap (M1,unM1) $ gXMLPickler

instance GFromToXML U1 where
	gXMLPickler = xpLift U1

instance (FromToXML a) => GFromToXML (K1 R a) where
	gXMLPickler = xpWrap (K1,unK1) xMLPickler

{-|
FromToXML declares that there is a pickler for the instance type
wrapped by the generic representation's K1 constructor.
We give instances for data types that we do want to encode in more precise way, not
following the generic pattern. For example, we want to encode unit "()" as a
distinct tag <UNIT/>, not as text content generated with show/read.
We give a default signature and definition which we can use conveniently by
just stating "instance Word32" for numeric types, for example.
-}
class FromToXML a where
	-- | An instance of FromToXML provides a pickler:
	xMLPickler :: Pickler a
	-- | The default signature of xMLPickler requires Read and Show instances...
	default xMLPickler :: (Read a,Show a) => Pickler a
	-- | ... because the default definition via xpPrim uses show and read to convert the value to XML tag text content.
	xMLPickler = xpContent xpPrim

instance FromToXML () where
	xMLPickler = xpElemNodes "UNIT" xpUnit

instance (FromToXML a) => FromToXML [a] where
	xMLPickler = xpElemNodes "LIST" $ xpList0 $ xpElemNodes "ITEM" xMLPickler

instance FromToXML Int
instance FromToXML Int8
instance FromToXML Int16
instance FromToXML Int32
instance FromToXML Int64
instance FromToXML Integer
instance FromToXML Word
instance FromToXML Word8
instance FromToXML Word16
instance FromToXML Word32
instance FromToXML Word64
instance FromToXML Float
instance FromToXML Double
instance (Show a,Read a,Integral a) => FromToXML (Ratio a)
instance (Show a,Read a) => FromToXML (Complex a)

instance FromToXML Bool where
	xMLPickler = xpAlt selfun [
		xpElemNodes "TRUE"  $ xpLift True,
		xpElemNodes "FALSE" $ xpLift False ] where
		selfun True  = 0
		selfun False = 1

{-|
We represent special Char's by Haskell's escape sequences.
A Char is not embraced by single quotes (as it would be using the default instance).
-}
instance FromToXML Char where
	xMLPickler = xpContent $ xpWrap ( fst.head.readLitChar, (`showLitChar` "") ) xpText

{-|
A String is represented a tag's text content, using the common Haskell escape sequences.
For better readability and prevention of very long lines in the XML file, after each '\r', '\n' and '\t'
the same character is inserted unescaped after the character's escape sequence.
The unescaped character is removed again when parsing the String.
For example, "abc\ndef" will be written in the XML file as "abc\\n\ndef",
with the LF escaped (i.e. "\\n") and followed by a "real" LF ('\n').
Injecting line breaks and tabs makes long text much more readable und humanely editable
(which is one of the original purposes of XML).
The "real" LF will be filtered out again while parsing the XML file String.
-}
instance FromToXML String where
	xMLPickler = xpContent pickleContentString

skipPickleString = (`elem` ['\r','\n','\t'])

{-|
Pickles a string to/from text content with escaping 
-}
pickleContentString :: PU String String
pickleContentString = xpWrap ( readLitString, showLitString ) xpText0 where
	readLitString "" = ""
	readLitString (c:ss) | skipPickleString c = readLitString ss
	readLitString s  = let [(c,ss)] = readLitChar s in c : readLitString ss
	showLitString "" = ""
	showLitString (c:ss) | skipPickleString c = showLitChar c $ c : showLitString ss
	showLitString (c:ss) = showLitChar c $ showLitString ss

{-|
Pickler for a Map.
Didn't use xpMap because show'ing keys as attributes might be inconvenient/unreadable for more complex key types.
-}
instance (Ord k,FromToXML k,FromToXML v) => FromToXML (Map.Map k v) where
	xMLPickler = xpElemNodes "MAP" $ xpWrap (Map.fromList,Map.toList) $ xpList $ 
		xpElemNodes "ASSOC" $ xpPair
			(xpElemNodes "KEY"  xMLPickler)
			(xpElemNodes "ELEM" xMLPickler)

{-|
Pickler for an IntMap.
Here we use an attribute for the Int key.
-}
instance (FromToXML v) => FromToXML (IntMap.IntMap v) where
	xMLPickler = xpElemNodes "INTMAP" $ xpWrap (IntMap.fromList,IntMap.toList) $ xpList $ 
		xpElem "ELEM" (xpAttr "index" xpPrim) xMLPickler

-- | Didn't use an attribute for the dimension of the tuple because this is not representable/checkable by a schema.
instance (FromToXML a,FromToXML b) => FromToXML (a,b) where
	xMLPickler = xpElemNodes "PAIR" $ xpPair (xpComponent 0) (xpComponent 1)

instance (FromToXML a,FromToXML b,FromToXML c) => FromToXML (a,b,c) where
	xMLPickler = xpElemNodes "TRIPLE" $ xpTriple (xpComponent 0) (xpComponent 1) (xpComponent 2)

instance (FromToXML a,FromToXML b,FromToXML c,FromToXML d) => FromToXML (a,b,c,d) where
	xMLPickler = xpElemNodes "QUADRUPLE" $ xp4Tuple (xpComponent 0) (xpComponent 1) (xpComponent 2) (xpComponent 3)

instance (FromToXML a,FromToXML b,FromToXML c,FromToXML d,FromToXML e) => FromToXML (a,b,c,d,e) where
	xMLPickler = xpElemNodes "QUINTUPLE" $ xp5Tuple (xpComponent 0) (xpComponent 1) (xpComponent 2) (xpComponent 3) (xpComponent 4)

instance (FromToXML a,FromToXML b,FromToXML c,FromToXML d,FromToXML e,FromToXML f) => FromToXML (a,b,c,d,e,f) where
	xMLPickler = xpElemNodes "SEXTUPLE" $ xp6Tuple (xpComponent 0) (xpComponent 1) (xpComponent 2) (xpComponent 3) (xpComponent 4) (xpComponent 5)

-- | Pickle a tuple's i'th component. The component tag names are distinct, otherwise hexpat-pickle confuses this with a list and gives a parse error.
xpComponent :: (FromToXML a) => Int -> Pickler a
xpComponent i = xpElemNodes (componentnames!!i) xMLPickler where
	componentnames = [ "FIRST","SECOND","THIRD","FOURTH","FIFTH","SIXTH" ]

instance (FromToXML a) => FromToXML (Maybe a) where
	xMLPickler = xpAlt selfun [
		xpElemNodes "NOTHING" $ xpLift Nothing,
		xpElemNodes "JUST"    $ xpWrap (Just, \ (Just a) -> a ) xMLPickler ] where
		selfun Nothing  = 0
		selfun (Just _) = 1

instance (FromToXML a,FromToXML b) => FromToXML (Either a b) where
	xMLPickler = xpAlt selfun [
		xpElemNodes "LEFT"  $ xpWrap ( Left,  \ (Left  a) -> a ) xMLPickler,
		xpElemNodes "RIGHT" $ xpWrap ( Right, \ (Right b) -> b ) xMLPickler ] where
		selfun (Left  _) = 0
		selfun (Right _) = 1

instance (FromToXML a,Ord a) => FromToXML (Set.Set a) where
	xMLPickler = xpElemNodes "SET" $ xpWrap (Set.fromList,Set.toList) $ xpList $
		xpElemNodes "ELEM" xMLPickler

-- TODO: Insert FromToXML instances for date and time types, maybe in conformance to XSD types?

-- | Here we assume that array index types (usually Int) are sufficiently simple to be represented as string in an attribute...
instance (Ix i,Show i,Read i,FromToXML e) => FromToXML (Array i e) where
	xMLPickler = xpWrap (list2arr,arr2list) $ xpElem "ARRAY" xpbounds $ xpList $
		xpElemNodes "ELEM" xMLPickler where
			xpbounds = xpPair (xpAttr "lowerBound" xpPrim) (xpAttr "upperBound" xpPrim)
			arr2list arr = (bounds arr,elems arr)
			list2arr (bounds,arrelems) = listArray bounds arrelems

-- | This is the catch-all instance, leading to the generic (Rep a) representation
instance (Generic a,GFromToXML (Rep a)) => FromToXML a where
	xMLPickler = xpWrap (to,from) gXMLPickler

{-|
Converts generic Haskell data to a (strict) ByteString containing the XML representation.
Usually the data type is made deriving Generic using the DeriveGeneric flag.
-}
toXML :: (Generic a,GFromToXML (Rep a)) => a -> BS.ByteString
toXML x = Format.format' $ Format.indent 2 $ pickleTree (xpRoot gXMLPickler) (from x)

{-|
Construct generic Haskell data from a (strict) ByteString containing the XML representation.
fromXMLEither will return Left in case of a parsing error, Right otherwise.
-}
fromXMLEither :: (GFromToXML f) => BS.ByteString -> Either String (f p)
fromXMLEither bs = case parse' defaultParseOptions bs of
	Left (XMLParseError errmsg loc) -> Left $ printf "XMLParseError at %s:\n    %s" (show loc) errmsg
	Right tree  -> unpickleTree' (xpRoot gXMLPickler) tree

{-|
Convenience function wrapping around fromXMLEither, throwing an error in case of a parsing error.
-}
fromXML :: (Generic a,GFromToXML (Rep a)) => BS.ByteString -> a
fromXML bs = case fromXMLEither bs of
	Left  errmsg -> error errmsg
	Right x      -> to x

{-|
Convenience action, writing an XML representation of generic Haskell data to a file.
-}
writeFileToXML :: (Generic a,GFromToXML (Rep a)) => FilePath -> a -> IO ()
writeFileToXML filepath a = BS.writeFile filepath $ toXML a

{-|
Convenience action, reading generic Haskell data from an XML file.
-}
readFileFromXML :: (Generic a,GFromToXML (Rep a)) => FilePath -> IO a
readFileFromXML filepath = BS.readFile filepath >>= return . fromXML
