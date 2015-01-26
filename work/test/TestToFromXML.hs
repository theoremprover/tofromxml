{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fcontext-stack=50 #-}

module Main where

import Text.XML.ToFromXML

import System.Exit
import Data.Char
import qualified Data.Map.Lazy as Map
import qualified Data.IntMap as IntMap
import qualified Data.Set as Set
import Data.Array.IArray
import Data.Complex
import Data.Ratio
import Text.Printf
import System.FilePath
import Control.Exception

testTest :: (Eq i,Eq e,Eq a,Ix i,Ord a,Show a,Show i,Read i,Show e,ToFromXML e,ToFromXML a) => (Int,Test a i e) -> IO Bool
testTest (i,test) = do
	putStrLn $ "\n--- Test " ++ show i ++ " : " ++ take 50 (show test)

	let testfilename = "Test" ++ show i ++ ".xml"

	writeToXMLFile testfilename test

	test' <- readFromXMLFile testfilename

	let ok = test==test'
	case ok of
		True -> putStrLn "OK."
		False -> putStrLn $ printf "/= ERROR!\nWRITTEN: %s\n/=\nREAD   : %s" (show test) (show test')
	return ok

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
	Test16 [TestSel] |
	Test17 { first::String, second::Int, third::Float } |
	Test18 Int Char Bool String |
	Test19 Int Char Bool String Int |
	Test20 Int Char Bool (String,Char) Int Float |
	Test21 Int Char Bool (String,Char,Int) Int Float Int Double Bool Char |
	Test22 (Ratio Int) |
	Test23 (Ratio Int,Complex Double) |
	Test24 Int Double |
	Test25 () Double
	deriving (Show,Eq,Generic)

testfilter ts = [ts!!4]  -- Just execute single test
--testfilter = id  -- Execute all tests


data TestSel = TestSel1 Int | TestSel2 Char | TestSel3 TestSel | TestSel4 (Test () Int ()) | TestSel5
	deriving (Show,Eq,Generic)

main = do
	longstring <- readFile $ "test" </> "TestToFromXML.hs"
	oks <- sequence $ map testTest $ zip [1..] (testfilter [
		Test13 (longstring,0),
		Test3 [(map chr [8..16],99)],
{-		Test4 [
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
			Test7 [1..10] ], -}
		Test8 $ Map.fromList [("abc",Test4 []),("def",Test5),("ghi",Test7 [1,2,3])],
		Test4 [ Test3 [("first",2)], Test6 (1.23,1.23456,False) ],
		Test9 (Just $ Test7 [1,2,3]) Nothing (Left $ Test1 7 'v' ()) (Right 8),
		Test10 (Set.fromList [10,27,-1,1,3,5,7,9]),
		Test11 (listArray (10,12) [3.3,4.4,5.5]),
		Test12 (27 % 8) (1.23 :+ (-3.45)),
		Test23 (27 % 8, 1.23 :+ (-3.45)),
		Test24 123 123.456,
		Test25 () 123.456,
		Test22 (27 % 8),
		Test15 $ IntMap.fromList $ zip [-2..] [Test5, Test7 [-2,-1,0], Test13 ("test15",666) ],
		Test16 [
			TestSel1 4,
			TestSel2 'f',
			TestSel3 $ TestSel3 $ TestSel3 $ TestSel1 9999,
			TestSel4 $ Test16 [TestSel1 5, TestSel4 $ Test5, TestSel5 ],
			TestSel5 ],
		Test17 "abc" 123 3.14,
		Test1 12 'c' (),
		Test18 13 'd' False "test18",
		Test19 13 'd' False "test19" 14,
		Test20 13 'd' False ("test20",'a') 15 2.71,
		Test21 13 'd' False ("test21",'b',3) 15 2.71 (-1) 3.1 True 'e'
		] :: [Test Integer Int Float])
	rc <- case all id oks of
		True -> putStrLn "All tests OK." >> return ExitSuccess
		False -> putStrLn "Some tests failed!" >> return (ExitFailure 1)
	exitWith rc