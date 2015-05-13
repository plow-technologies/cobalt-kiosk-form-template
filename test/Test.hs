{-# LANGUAGE OverloadedStrings #-}

module Main where

import Kiosk.Backend.Form

import Data.Attoparsec.Text
import System.Exit (exitFailure) 
-- (exitFailure, exitSuccess)
--import qualified Data.Text as T

main :: IO ()
main = do
	{-
	print $ parseOnly parseXML "<abc></abc>   "
	print $ parseOnly parseXML "<abc data='1'></abc>   "
	print $ parseOnly parseXML "<abc   data='1'  boy='girl'></abc>   "
	print $ parseOnly parseXML "<abc   data='1'  boy='girl'> <div class='ngClass'>  </div> </abc>   "
	let a = toAttribute $ WidthAttribute 1
	print a
	-}
	-- print $ parseOnly (parseAttributesZ "abc") " abc='123'"
	-- print $ parseOnly (parseAttributesZ "abc") " abce='123'"
	-- print $ parseOnly (parseAttributesZ "abc") " ab='123'"
	
	--let z = Attribute "width" (T.pack ("'" ++ show 1 ++ "'"))
	--print z
	--let x = fromAttribute (z :: Attribute)
	--print "Hello"
	--let t = fromAttribute ( Attribute "maxd" "1.0")
	let x = parseOnly (parseElement "input") "<input type='double' indexable='True'>3.3</input>"
	case x of
		Right x' -> do 
			--let y = (genericAttributeDecoder $ attributes x')
			print x'
			--let z = (genericAttributeDecoder $ attributes x')
			let y = (parseInputType (genericAttributeDecoder $ attributes x') "input")
			print y
			print "hello"
		Left _ -> print "didn't parse"
	--print $ parseInputType (genericAttributeDecoder attr) elemVal
	print $ parseOnly inputParser "<input type='double' indexable='True'>3.3</input>"
	exitFailure