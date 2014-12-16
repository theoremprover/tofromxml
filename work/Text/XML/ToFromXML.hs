{-# LANGUAGE DeriveGeneric,DefaultSignatures,UndecidableInstances,ScopedTypeVariables,
	OverlappingInstances,TypeOperators,FlexibleContexts,FlexibleInstances #-}

module Text.XML.ToFromXML (
	FromToXML,GFromToXML,
	toXML,fromXML,fromXMLEither,
	readFileFromXML,writeFileToXML,
	module GHC.Generics
	) where

import GHC.Generics

import Debug.Trace

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


type Pickler a = PU (ListOf (UNode String)) a

class GFromToXML f where
	gXMLPickler :: Pickler (f p)

instance (GFromToXML f1,GFromToXML f2) => GFromToXML (f1 :*: f2) where
	gXMLPickler = xpWrap (uncurry (:*:),\ (a :*: b) -> (a,b)) $
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

instance (GFromToXML f,Constructor c) => GFromToXML (M1 C c f) where
	gXMLPickler = xpWrap (M1,unM1) $ xpElemName "CONSTRUCTOR" conname gXMLPickler where
		conname = conName (undefined :: M1 C c f p)

instance (GFromToXML f,Selector s) => GFromToXML (M1 S s f) where
	gXMLPickler = xpWrap (M1,unM1) $ xpElemName "SELECTOR" selname gXMLPickler where
		selname = selName (undefined :: M1 S s f p)

xpElemName tag attrval pickler = xpWrap (snd,\b->((),b)) $
	xpElem tag (xpAttrFixed "name" attrval) pickler

{-
V1 is not an instance of GFromToXML, because we can't serialize data of an empty type since there
are no constructors. It shouldn't be necessary to serialize an empty type, hence this should be rejected by the type checker.
-}

instance (GFromToXML f) => GFromToXML (M1 S NoSelector f) where
	gXMLPickler = xpWrap (M1,unM1) $ gXMLPickler

instance GFromToXML U1 where
	gXMLPickler = xpLift U1

instance (FromToXML a) => GFromToXML (K1 R a) where
	gXMLPickler = xpWrap (K1,unK1) xMLPickler

class FromToXML a where
	xMLPickler :: Pickler a
	default xMLPickler :: (Read a,Show a) => Pickler a
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

instance FromToXML Char where
	xMLPickler = xpContent $ xpWrap ( fst.head.readLitChar, (`showLitChar` "") ) xpText

instance FromToXML String where
	xMLPickler = xpContent pickleContentString

pickleLiterally = (`elem` ['\r','\n','\t'])

pickleContentString :: PU String String
pickleContentString = xpWrap ( readLitString, showLitString ) xpText0 where
	readLitString "" = ""
	readLitString (c:ss) | pickleLiterally c = c : readLitString ss
	readLitString s  = let [(c,ss)] = readLitChar s in c : readLitString ss
	showLitString "" = ""
	showLitString (c:ss) | pickleLiterally c = c : showLitString ss
	showLitString (c:ss) = showLitChar c $ showLitString ss

-- Didn't use xpMap because show'ing keys as attributes might be inconvenient/unreadable for more complex key types
instance (Ord k,FromToXML k,FromToXML v) => FromToXML (Map.Map k v) where
	xMLPickler = xpElemNodes "MAP" $ xpWrap (Map.fromList,Map.toList) $ xpList $ 
		xpElemNodes "ASSOC" $ xpPair
			(xpElemNodes "KEY"  xMLPickler)
			(xpElemNodes "ELEM" xMLPickler)

instance (FromToXML v) => FromToXML (IntMap.IntMap v) where
	xMLPickler = xpElemNodes "INTMAP" $ xpWrap (IntMap.fromList,IntMap.toList) $ xpList $ 
		xpElem "ELEM" (xpAttr "index" xpPrim) xMLPickler

-- Didn't use an attribute for the dimension of the tuple because this is not representable/checkable by a schema

instance (FromToXML a,FromToXML b) => FromToXML (a,b) where
	xMLPickler = xpElemNodes "TUPLE-2" $ xpPair (xpComponent 1) (xpComponent 2)

instance (FromToXML a,FromToXML b,FromToXML c) => FromToXML (a,b,c) where
	xMLPickler = xpElemNodes "TUPLE-3" $ xpTriple (xpComponent 1) (xpComponent 2) (xpComponent 3)

instance (FromToXML a,FromToXML b,FromToXML c,FromToXML d) => FromToXML (a,b,c,d) where
	xMLPickler = xpElemNodes "TUPLE-4" $ xp4Tuple (xpComponent 1) (xpComponent 2) (xpComponent 3) (xpComponent 4)

instance (FromToXML a,FromToXML b,FromToXML c,FromToXML d,FromToXML e) => FromToXML (a,b,c,d,e) where
	xMLPickler = xpElemNodes "TUPLE-5" $ xp5Tuple (xpComponent 1) (xpComponent 2) (xpComponent 3) (xpComponent 4) (xpComponent 5)

instance (FromToXML a,FromToXML b,FromToXML c,FromToXML d,FromToXML e,FromToXML f) => FromToXML (a,b,c,d,e,f) where
	xMLPickler = xpElemNodes "TUPLE-6" $ xp6Tuple (xpComponent 1) (xpComponent 2) (xpComponent 3) (xpComponent 4) (xpComponent 5) (xpComponent 6)

xpComponent :: (FromToXML a) => Int -> Pickler a
xpComponent i = xpElemNodes ("COMPONENT-" ++ show i) xMLPickler

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
	xMLPickler = xpElemNodes "SET" $ xpWrap (Set.fromList,Set.toList) $ xpList $ xpElemNodes "ELEM" xMLPickler

-- TODO: Insert FromToXML instances for date and time types, maybe in conformance to XSD types?

-- Here we assume that array index types are sufficiently simple to be represented as string in an attribute...
instance (Ix i,Show i,Read i,FromToXML e) => FromToXML (Array i e) where
	xMLPickler = xpWrap (list2arr,arr2list) $ xpElem "ARRAY" xpbounds $ xpList $ xpElemNodes "ELEM" xMLPickler where
		xpbounds = xpPair (xpAttr "lowerBound" xpPrim) (xpAttr "upperBound" xpPrim)
		arr2list arr = (bounds arr,elems arr)
		list2arr (bounds,arrelems) = listArray bounds arrelems

-- This is the catch-all instance, leading to the generic (Rep a) representation
instance (Generic a,GFromToXML (Rep a)) => FromToXML a where
	xMLPickler = xpWrap (to,from) gXMLPickler

toXML :: (Generic a,GFromToXML (Rep a)) => a -> BS.ByteString
toXML x = Format.format' $ Format.indent 2 $ pickleTree (xpRoot gXMLPickler) (from x)

fromXMLEither :: (GFromToXML f) => BS.ByteString -> Either String (f p)
fromXMLEither bs = case parse' defaultParseOptions bs of
	Left (XMLParseError errmsg loc) -> Left $ printf "XMLParseError at %s:\n    %s" (show loc) errmsg
	Right tree  -> unpickleTree' (xpRoot gXMLPickler) tree

fromXML :: (Generic a,GFromToXML (Rep a)) => BS.ByteString -> a
fromXML bs = case fromXMLEither bs of
	Left  errmsg -> error errmsg
	Right x      -> to x

writeFileToXML :: (Generic a,GFromToXML (Rep a)) => FilePath -> a -> IO ()
writeFileToXML filepath a = BS.writeFile filepath $ toXML a

readFileFromXML :: (Generic a,GFromToXML (Rep a)) => FilePath -> IO a
readFileFromXML filepath = BS.readFile filepath >>= return . fromXML
