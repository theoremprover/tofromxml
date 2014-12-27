{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fcontext-stack=50 #-}

import Text.XML.ToFromXML
-- GHC.Generics is exported by ToFromXML

data Test = Test { str::String, something::(Int,Char) }
	deriving (Generic,Show,Eq)

main = do
	let test = Test "abc" (42,'z')
	writeToXMLFile "test.xml" test
	putStrLn $ "writeToXMLFile : " ++ show test

	-- readFromXMLFile's return type can be inferred in this example,
	-- otherwise it would have to be declared
	test' <- readFromXMLFile "test.xml"
	putStrLn $ "readFromXMLFile: " ++ show test'
	
	putStrLn $ if test==test' then "OK." else "ERROR!"
