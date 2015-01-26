{-# LANGUAGE DeriveGeneric,DefaultSignatures,UndecidableInstances,ScopedTypeVariables,
	OverlappingInstances,TypeOperators,FlexibleContexts,FlexibleInstances,MultiParamTypeClasses,TypeFamilies,AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -fcontext-stack=50 #-}

{-|
Module      : Text.XML.ToFromXML
Description : A library for reading and writing Haskell data from/to XML files
Copyright   : (c) Robert Reitmeier, 2014
License     : GPL-3
Maintainer  : concat ["haskell","< a t >","thinking","-","machines",".","net"]
Stability   : provisional, tested
Portability : GHC

In order to represent a Haskell value in an XML file, the value's type just has to be an instance of 'Generic'
(usually one would use the @deriving 'Generic'@ clause in the @data@ defintion).
Given that, 'writeToXMLFile' will write an XML file with a representation of the value,
which can be reconstructed again by using 'readFromXMLFile'. See the following example
(pragma syntax below is broken, how can one correctly incorporate multiline comments in a haddock code block?):

>{ -# LANGUAGE DeriveGeneric #- }
>{ -# OPTIONS_GHC -fcontext-stack=50 #- }
>
>import Text.XML.ToFromXML
>-- GHC.Generics is exported by ToFromXML
>
>data Test = Test { str::String, something::(Int,Char) }
>	deriving (Generic,Show,Eq)
>
>main = do
>	let test = Test "abc" (42,'z')
>	writeToXMLFile "test.xml" test
>	putStrLn $ "writeToXMLFile : " ++ show test
>
>	-- readFromXMLFile's return type can be inferred in this example,
>	-- otherwise it would have to be declared
>	test' <- readFromXMLFile "test.xml"
>	putStrLn $ "readFromXMLFile: " ++ show test'
>	
>	putStrLn $ if test==test' then "OK." else "ERROR!"

The generated XML file reads

><?xml version="1.0" encoding="UTF-8"?>
><CONSTRUCTOR name="Test">
>  <ARG-1 selector="str">abc</ARG-1>
>  <ARG-2 selector="something">
>    <PAIR>
>      <FIRST>42</FIRST>
>      <SECOND>z</SECOND>
>    </PAIR>
>  </ARG-2>
></CONSTRUCTOR>

The general intention of this module is to keep the generated XML as intuitive and
easy to read as possible. For example, we do flatten nested pairs used by "GHC.Generics"
to represent n-tuples, and TABs and line breaks are inserted after their escaped representation,
so it keeps its formatting (unfortunately, CDATA sections are not supported with
the 'Node' types used in "Text.XML.Expat.Pickle").

Some strategic design decisions were made:
* We do not create new tags named after the data type names, since then we could not
  give a common schema for all XML files we are creating.
  Otherwise, we would have to create a schema

One might need to increase the context stack size with the @-fcontext-stack@ option
when compiling code using this module.

A test suite is included in the package, see <file:test/TestToFromXML.hs TestToFromXML.hs> in the package's test directory.
-}
module Text.XML.ToFromXML (
	Pickler,
	ToFromXML(..),
	toXML,fromXML,fromXMLEither,
	readFromXMLFile,writeToXMLFile,
	module GHC.Generics
	) where

import GHC.Generics

import qualified Data.ByteString as BS

import Text.XML.ToFromXML.Pickle --Text.XML.Expat.Pickle
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

import Debug.Trace --TODO

withATN = False

-- | For the picklers, we are using nodes with both tag and text being 'String's.
type Pickler a = PU (ListOf (UNode String)) a

class GToFromXML f where
	gXMLPickler :: Pickler (f p)

{-|
We do not want our XML to be cluttered with nested tuples, so we flatten
these in the XML representation.
-}
class PickleProdN f where
	-- | Given the current index in the flattened tuple, pickleProdN returns the incremented index and the pickler.
	pickleProdN :: Int -> (Int,Pickler (f p))

{-|
Pickling a product is done by first pickling the first component, and then pickling the second
component with the current index @i1@ returned by the first @pickleProdN i@.
The index @i2@ after @pickleProdN i1@ is returned.
-}
instance (PickleProdN f1,PickleProdN f2) => PickleProdN (f1 :*: f2) where
	pickleProdN i = (i2,xpTrace ("i = " ++ show i ++ ": PickleProdN (f1 :*: f2)") $ xpWrap (uncurry (:*:),\ (a :*: b) -> (a,b)) $ xpPair p1 p2) where
		(i1,p1) = pickleProdN i
		(i2,p2) = pickleProdN i1

argTagNames = {-repeat "ARG" -}[ "ARG-" ++ show i | i <- [1..] ]

instance (GToFromXML f) => PickleProdN (M1 S NoSelector f) where
	pickleProdN i = (i+1,xpTrace ("i = " ++ show i ++ ": PickleProdN (M1 S NoSelector f)") $ (if withATN then xpElemNodes (argTagNames!!i) else id) gXMLPickler)

{-|
For constructor arguments, one can also leave out the selector attribute
(like Haskell syntax allows leaving out record syntax).
This makes it easier to manually write data XML.
-}
instance (GToFromXML f,Selector s) => PickleProdN (M1 S s f) where
	pickleProdN i = (i+1,xpTrace ("i = " ++ show i ++ ", " ++ selname ++ ": PickleProdN (M1 S s f)") $ (if withATN then xpElemNodes (argTagNames!!i) else id) picklearg) where
		selname = selName (undefined :: M1 S s f p)
		picklearg = xpAlt (\_->0) [
--			xpElemWithAttr "SELECTOR" "name" selname gXMLPickler,
--			gXMLPickler ]
--			xpElemWithAttr (argTagNames!!i) "selector" selname gXMLPickler,
--			xpElemNodes (argTagNames!!i) gXMLPickler ]
			xpElemWithAttr "SELECTOR" "name" selname gXMLPickler,
			gXMLPickler ]

xpTrace :: (Show t) => String -> PU t a -> PU t a
xpTrace s xp = PU {
	unpickleTree  = unpickleTree xp,
	unpickleTree' = \ tree -> let up = trace (printf "unpickling' %s : %s" s (show tree)) $ unpickleTree' xp tree in
		trace (printf "unpickle' %s : => %s" s (showErrorOrSuccess up)) up,
	pickleTree    = pickleTree xp
	} where
	showErrorOrSuccess (Left errmsg) = "ERROR: " ++ errmsg
	showErrorOrSuccess (Right val)   = "SUCCESS: "

instance (PickleProdN (f1 :*: f2)) => GToFromXML (f1 :*: f2) where
	gXMLPickler = xpTrace "(f1 :*: f2)" $ snd (pickleProdN 0)

instance (GToFromXML f1,GToFromXML f2) => GToFromXML (f1 :+: f2) where
	gXMLPickler = xpTrace "GToFromXML (f1 :+: f2)" $ xpAlt selfun [
		xpWrap (L1,\ (L1 x) -> x) gXMLPickler,
		xpWrap (R1,\ (R1 x) -> x) gXMLPickler ] where
		selfun (L1 _) = 0
		selfun (R1 _) = 1

{-|
We won't construct a tag for @M1 D@, since it is unnecessary and cluttering the XML.
Although, it might add to safety...
-}
instance (GToFromXML f,Datatype d) => GToFromXML (M1 D d f) where
	gXMLPickler = xpTrace (datatypename ++ ": GToFromXML (M1 D d f)") $ xpWrap (M1,unM1) gXMLPickler where
		datatypename = datatypeName (undefined :: M1 D d f p)

{-|
The CONSTRUCTOR tag contains a name attribute with the constructor's name.
Strictly speaking it is unnecessary to encode the name as well, but improves readability
and might add to safety...
-}
instance (GToFromXML f,Constructor c) => GToFromXML (M1 C c f) where
	gXMLPickler = xpTrace (conname ++ ": GToFromXML (M1 C c f)") $ xpWrap (M1,unM1) $ xpElemNodes conname gXMLPickler where
		conname = conName (undefined :: M1 C c f p)

-- | No tag created for @M1 S NoSelector@, this is handled in 'PickleProdN'.
instance (GToFromXML f) => GToFromXML (M1 S NoSelector f) where
	gXMLPickler = xpTrace "NoSelector: GToFromXML (M1 S NoSelector f)" $ xpWrap (M1,unM1) gXMLPickler

-- | No tag created for @M1 S s@, this is handled in 'PickleProdN'.
instance (GToFromXML f,Selector s) => GToFromXML (M1 S s f) where
	gXMLPickler = xpTrace (selname ++ ": GToFromXML (M1 S s f)") $ xpWrap (M1,unM1) gXMLPickler where
		selname = selName (undefined :: M1 S s f p)

{-
@V1@ is not an instance of 'GToFromXML', because we can't serialize data of an empty type since there
are no constructors. It shouldn't be necessary to serialize an empty type, so @V1@ should be rejected by the type checker.
-}

instance GToFromXML U1 where
	gXMLPickler = xpTrace "GToFromXML U1" $ xpLift U1

instance (ToFromXML a) => GToFromXML (K1 R a) where
	gXMLPickler = xpTrace "GToFromXML (K1 R a)" $ xpWrap (K1,unK1) xMLPickler

-- | A helper function injecting an attribute with a given/fixed value.
xpElemWithAttr tag attrname attrval pickler = xpWrap (snd,\b->((),b)) $
	xpElem tag (xpAttrFixed attrname attrval) pickler

{-|
The class 'ToFromXML' declares that there is a pickler for the instance type.

We give a default signature and definition which uses textual representation via show/read.
This can be used conveniently by just stating @instance ToFromXML Word32@, for numeric types, for example.

We also define instances for data types that we do want to encode in a more concise way, not
following the pattern that is created by "GHC.Generics". For example, we want to encode unit @()@ as a
distinct tag @\<UNIT\/\>@, not as text content that would be generated by the default instance.
Also, we flatten nested pairs constructed by "GHC.Generics" to represent n-tuples.

Special chars are represented in the tag content by Haskell's escape sequences, not
embraced by single quotes (as it would be using the default instance).

Since unfortunately CDATA sections are not supported with
the Node types used in "Text.XML.Expat.Pickle" we are relying on,
a String has to be represented in XML as a tag's text content, using the common Haskell escape sequences.
For better readability and prevention of very long lines in the XML file, after each @'\r'@, @'\n'@ and @'\t'@
the same character is inserted unescaped after the character's escape sequence.
The unescaped characters are removed again when parsing the String.
For example, @"abc\\ndef"@ will be written in the XML file as @"abc\\\\n\\ndef"@,
with the LF escaped (i.e. @"\\\\n"@) and followed by a real LF (@'\n'@).
Injecting line breaks and tabs makes long text much more readable and editable by humans
(which is one of the original purposes of XML).
The inserted newlines and TABs will be filtered out again while parsing the String.
-}
class ToFromXML a where
	-- | An instance of 'ToFromXML' provides a pickler for type @a@.
	xMLPickler :: Pickler a

-- | A pickler using show and read to convert a value to XML tag text content.
readShowPickler :: (Read a,Show a) => String -> Pickler a
readShowPickler tagname = xpTrace tagname $ xpElemNodes tagname $ xpContent $ xpTrace "PRIM" $ xpPrim

instance ToFromXML () where
	xMLPickler = xpTrace "ToFromXML ()" $ xpElemNodes "Unit" xpUnit

instance (ToFromXML a) => ToFromXML [a] where
	xMLPickler = xpTrace "ToFromXML [a]" $ xpElemNodes "List" $ xpList0 xMLPickler

instance ToFromXML Int where xMLPickler = readShowPickler "Int"
instance ToFromXML Int8 where xMLPickler = readShowPickler "Int8"
instance ToFromXML Int16 where xMLPickler = readShowPickler "Int16"
instance ToFromXML Int32 where xMLPickler = readShowPickler "Int32"
instance ToFromXML Int64 where xMLPickler = readShowPickler "Int64"
instance ToFromXML Integer where xMLPickler = readShowPickler "Integer"
instance ToFromXML Word where xMLPickler = readShowPickler "Word"
instance ToFromXML Word8 where xMLPickler = readShowPickler "Word8"
instance ToFromXML Word16 where xMLPickler = readShowPickler "Word16"
instance ToFromXML Word32 where xMLPickler = readShowPickler "Word32"
instance ToFromXML Word64 where xMLPickler = readShowPickler "Word64"
instance ToFromXML Float where xMLPickler = readShowPickler "Float"
instance ToFromXML Double where xMLPickler = readShowPickler "Double"
instance (Show a,Read a,Integral a) => ToFromXML (Ratio a) where xMLPickler = readShowPickler "Ratio"
instance (Show a,Read a) => ToFromXML (Complex a) where xMLPickler = readShowPickler "Complex"

instance ToFromXML Bool where
	xMLPickler = xpTrace "Bool" $ xpAlt selfun [
		xpElemNodes "True"  $ xpLift True,
		xpElemNodes "False" $ xpLift False ] where
		selfun True  = 0
		selfun False = 1

instance ToFromXML Char where
	xMLPickler = xpTrace "Char" $ xpElemNodes "Char" $ xpContent $ xpWrap ( fst.head.readLitChar, (`showLitChar` "") ) xpText

instance ToFromXML String where
	xMLPickler = xpTrace "String" $ xpElemNodes "String" $ xpContent pickleContentString

skipPickleString = (`elem` ['\r','\n','\t'])

{-|
Pickles a string to\/from text content with escaping 
-}
pickleContentString :: PU String String
pickleContentString = xpWrap ( readLitString, showLitString ) xpText0 where
	readLitString "" = ""
	readLitString (c:ss) | skipPickleString c = readLitString ss
	readLitString s  = let [(c,ss)] = readLitChar s in c : readLitString ss
	showLitString "" = ""
	showLitString (c:ss) | skipPickleString c = showLitChar c $ c : showLitString ss
	showLitString (c:ss) = showLitChar c $ showLitString ss

{-
Pickler for a Map.
Didn't use 'Text.XML.Expat.Pickle.xpMap' because @show@ing keys as attributes might be inconvenient/unreadable for more complex key types.
Otherwise, one would probably use an @Int@ as key, so one should use "Data.IntMap" anyway.
-}
instance (Ord k,ToFromXML k,ToFromXML v) => ToFromXML (Map.Map k v) where
	xMLPickler = xpElemNodes "MAP" $ xpWrap (Map.fromList,Map.toList) $ xpList0 $ 
		xpElemNodes "ASSOC" $ xpPair xMLPickler xMLPickler

instance (ToFromXML v) => ToFromXML (IntMap.IntMap v) where
	xMLPickler = xpElemNodes "IntMap" $ xpWrap (IntMap.fromList,IntMap.toList) $ xpList0 $ 
		xpElem "ELEM" (xpAttr "key" xpPrim) xMLPickler

-- Didn't use an attribute for the dimension of the tuple because this is not checkable by a schema.
instance (ToFromXML a,ToFromXML b) => ToFromXML (a,b) where
	xMLPickler = xpElemNodes "PAIR" $ xpPair (xpComponent 0) (xpComponent 1)

instance (ToFromXML a,ToFromXML b,ToFromXML c) => ToFromXML (a,b,c) where
	xMLPickler = xpElemNodes "TRIPLE" $ xpTriple (xpComponent 0) (xpComponent 1) (xpComponent 2)

instance (ToFromXML a,ToFromXML b,ToFromXML c,ToFromXML d) => ToFromXML (a,b,c,d) where
	xMLPickler = xpElemNodes "QUADRUPLE" $ xp4Tuple (xpComponent 0) (xpComponent 1) (xpComponent 2) (xpComponent 3)

instance (ToFromXML a,ToFromXML b,ToFromXML c,ToFromXML d,ToFromXML e) => ToFromXML (a,b,c,d,e) where
	xMLPickler = xpElemNodes "QUINTUPLE" $ xp5Tuple (xpComponent 0) (xpComponent 1) (xpComponent 2) (xpComponent 3) (xpComponent 4)

instance (ToFromXML a,ToFromXML b,ToFromXML c,ToFromXML d,ToFromXML e,ToFromXML f) => ToFromXML (a,b,c,d,e,f) where
	xMLPickler = xpElemNodes "SEXTUPLE" $ xp6Tuple (xpComponent 0) (xpComponent 1) (xpComponent 2) (xpComponent 3) (xpComponent 4) (xpComponent 5)

-- | Pickle a tuple's @i@\'th component. The component tag names are distinct, otherwise hexpat-pickle confuses this with a list and gives a parse error.
xpComponent :: (ToFromXML a) => Int -> Pickler a
xpComponent i = xMLPickler --xpElemNodes (componentnames!!i) xMLPickler where
	--componentnames = [ "FIRST","SECOND","THIRD","FOURTH","FIFTH","SIXTH" ]

instance (ToFromXML a) => ToFromXML (Maybe a) where
	xMLPickler = xpTrace "ToFromXML (Maybe a)" $ xpAlt selfun [
		xpElemNodes "Nothing" $ xpLift Nothing,
		xpElemNodes "Just"    $ xpWrap (Just, \ (Just a) -> a ) xMLPickler ] where
		selfun Nothing  = 0
		selfun (Just _) = 1

instance (ToFromXML a,ToFromXML b) => ToFromXML (Either a b) where
	xMLPickler = xpTrace "ToFromXML (Either a b)" $ xpAlt selfun [
		xpElemNodes "Left"  $ xpWrap ( Left,  \ (Left  a) -> a ) xMLPickler,
		xpElemNodes "Right" $ xpWrap ( Right, \ (Right b) -> b ) xMLPickler ] where
		selfun (Left  _) = 0
		selfun (Right _) = 1

instance (ToFromXML a,Ord a) => ToFromXML (Set.Set a) where
	xMLPickler = xpElemNodes "Set" $ xpWrap (Set.fromList,Set.toList) $ xpList0 xMLPickler
--		xpElemNodes "ELEM" xMLPickler

-- TODO: Insert ToFromXML instances for date and time types, maybe in conformance to XSD types?

-- Here we assume that array index types (usually 'Int') are sufficiently simple to be represented as string in an attribute.
instance (Ix i,Show i,Read i,ToFromXML e) => ToFromXML (Array i e) where
	xMLPickler = xpWrap (list2arr,arr2list) $ xpElem "Array" xpbounds $ xpList0 $ xMLPickler where
		xpbounds = xpPair (xpAttr "lowerBound" xpPrim) (xpAttr "upperBound" xpPrim)
		arr2list arr = (bounds arr,elems arr)
		list2arr (bounds,arrelems) = listArray bounds arrelems

-- This is the catch-all instance, leading to the generic @Rep a@ representation
instance (Generic a,GToFromXML (Rep a)) => ToFromXML a where
	xMLPickler = xpTrace "catchall: ToFromXML a" $ xpWrap (to,from) gXMLPickler

{-|
Converts generic Haskell data to a (strict) 'ByteString' containing the XML representation.
In most cases, the data type would probably be @deriving 'Generic'@ using the @DeriveGeneric@ language pragma.

Remark: Both 'writeToXMLFile' and 'readFromXMLFile' require
@a@'s generic represention @Rep a@ be an instance of @GFromToXML@, but this is implemented in
this library (i.e. automatically fulfilled).
-}
toXML :: (Generic a,GToFromXML (Rep a)) => a -> BS.ByteString
toXML x = Format.format' $ Format.indent 2 $ pickleTree (xpRoot gXMLPickler) (from x)

{-|
Construct generic Haskell data from a (strict) 'ByteString' containing the XML representation.
fromXMLEither will return 'Left' in case of a 'XMLParseError', 'Right' otherwise.
-}
fromXMLEither :: (GToFromXML f) => BS.ByteString -> Either String (f p)
fromXMLEither bs = case parse' defaultParseOptions bs of
	Left (XMLParseError errmsg loc) ->
		Left $ printf "XMLParseError at %s:\n    %s" (show loc) errmsg
	Right tree ->
		unpickleTree' (xpRoot gXMLPickler) tree

{-|
Convenience function wrapping around 'fromXMLEither', throwing an error in case of a parsing error.
-}
fromXML :: (Generic a,GToFromXML (Rep a)) => BS.ByteString -> a
fromXML bs = case fromXMLEither bs of
	Left  errmsg -> error errmsg
	Right x      -> to x

{-|
Action writing an XML representation of generic Haskell data to a file.
The underlying writeFile operation is strict.
-}
writeToXMLFile :: (Generic a,GToFromXML (Rep a)) => FilePath -> a -> IO ()
writeToXMLFile filepath a = BS.writeFile filepath $ toXML a

{-|
Action reading generic Haskell data from an XML file.
The underlying readFile operation is strict.
-}
readFromXMLFile :: (Generic a,GToFromXML (Rep a)) => FilePath -> IO a
readFromXMLFile filepath = BS.readFile filepath >>= return . fromXML
