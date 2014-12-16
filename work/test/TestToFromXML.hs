{-# LANGUAGE DeriveGeneric #-}

module Main where

import Text.XML.ToFromXML
-- GHC.Generics is exported by ToFromXML

import System.Exit
import Data.Char
import qualified Data.Map.Lazy as Map
import qualified Data.IntMap.Lazy as IntMap
import qualified Data.Set as Set
import Data.Array.IArray
import Data.Complex
import Data.Ratio
import Text.Printf

testTest :: (Eq i,Eq e,Eq a,Ix i,Ord a,Show a,Show i,Read i,Show e,FromToXML e,FromToXML a) => (Int,Test a i e) -> IO Bool
testTest (i,test) = do
	putStrLn $ "\n-------------------------\nTest " ++ show i

	let testfilename = "Test" ++ show i ++ ".xml"

	writeFileToXML testfilename test

	test' <- readFileFromXML testfilename

	case test==test' of
		True -> putStrLn "OK."
		False -> error $ printf "/= ERROR!\nWRITTEN: %s\n/=\nREAD   : %s" (show test) (show test')
	return $ test==test'

data Test a i e =
	Test1 Int Char () |
	Test2 { str::String, subtest::(Test a i e) } |
	Test3 [(String,Int)] |
	Test4 [Test a i e] |
	Test5 |
	Test6 (Float,Double,Bool) |
	Test7 [Int] |
	Test8 (Map.Map String (Test a i e)) |
	Test9 (Maybe (Test a i e)) (Maybe Int) (Either (Test a i e) Char) (Either (Test a i e) Int) |
	Test10 (Set.Set a) |
	Test11 (Array i e) |
	Test12 (Ratio Int) (Complex Double) |
	Test13 (String,Int) |
	Test14 (Int,Int) |
	Test15 (IntMap.IntMap (Test () Int ())) |
	Test16 [TestSel]
	deriving (Show,Eq,Generic)

data TestSel = TestSel1 Int | TestSel2 Char | TestSel3 TestSel | TestSel4 (Test () Int ()) | TestSel5
	deriving (Show,Eq,Generic)

main = do
	longstring <- readFile "TestToFromXML.hs"
	oks <- sequence $ map testTest $ zip [1..] ([
		Test13 (longstring,0),
		Test3 [(map chr [8..16],99)],
		Test4 [
			Test4 $
				Test2 "  teststr  " (Test1 2 '\'' ()) :
					map (\ i -> Test1 (0-i) (chr i) ()) [0..255],
			Test3 [("<DATATYPE name=\"Test\">",100)],
			Test3 [(map chr [0..255],99)],
			Test13 ("abc",3),
			Test14 (3,4),
			Test5,
			Test4 [],
			Test3 [("",0)],
			Test6 (pi,pi,True),
			Test6 (1.2345678e+37,1.2345678e-250,False),
			Test7 [1..10] ],
		Test8 $ Map.fromList [("abc",Test4 []),("def",Test5),("ghi",Test7 [1,2,3])],
		Test4 [ Test3 [("first",2)], Test6 (1.23,1.23456,False) ],
		Test9 (Just $ Test7 [1,2,3]) Nothing (Left $ Test1 7 'v' ()) (Right 8),
		Test10 (Set.fromList [10,27,-1,1,3,5,7,9]),
		Test11 (listArray (10,12) [3.3,4.4,5.5]),
		Test12 (27 % 8) (1.23 :+ (-3.45)),
		Test15 $ IntMap.fromList $ zip [-2..] [Test5, Test7 [-2,-1,0], Test13 ("test15",666) ],
		Test16 [
			TestSel1 4,
			TestSel2 'f',
			TestSel3 $ TestSel3 $ TestSel3 $ TestSel1 9999,
			TestSel4 $ Test16 [TestSel1 5, TestSel4 $ Test5, TestSel5 ],
			TestSel5 ]
		] :: [Test Integer Int Float])
	rc <- case all id oks of
		True -> putStrLn "All tests OK." >> return ExitSuccess
		False -> putStrLn "Some tests failed!" >> return (ExitFailure 1)
	exitWith rc
