{-# LANGUAGE OverloadedStrings #-}

module Main where

import Kiosk.Backend.Form

import Control.Applicative
import Data.Attoparsec.Text
import System.Exit (exitFailure) 

main :: IO ()
main = do
	let b = parseOnly buttonParser "<button width='12' action='sendJson'></button>"
	print b
	let c =  parseOnly (parseElement "button") "<button width='12' action='sendJson'></button>"
	print c
	{-
	let l = parseOnly (parseElement "label") "<label width='12'>Legal Dest</label>"
	print l
	let ll = parseOnly labelParser "<label width='12'>Legal Dest</label>"
	print ll
	let b = parseOnly buttonParser "<button width='12' action='sendJson'></button>"
	print b
	
	let t = ( Attribute "maxd" "1.0")
	print t
	
	let e = genericAttributeDecoder [t] 
	-}
	{-
	let x = parseOnly (parseElement "input") "<input type='double' indexable='True'>3.3</input>"
	case x of
		Right x' -> do 
			--let y = (genericAttributeDecoder $ attributes x')
			print x'
			--let z = (genericAttributeDecoder $ attributes x')
			let y = (parseInputType (genericAttributeDecoder $ attributes x') (value x'))
			print y
			print "hello"
		Left _ -> print "didn't parse"
	--print $ parseInputType (genericAttributeDecoder attr) elemVal
	print $ parseOnly inputParser "<input type='double' indexable='True'>3.3</input>"
	-- "<input type='double' indexable='True'>3.3</input>"
	-}
	exitFailure