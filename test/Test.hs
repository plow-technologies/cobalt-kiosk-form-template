{-# LANGUAGE OverloadedStrings #-}

module Main where

import Kiosk.Backend.Form

import Control.Applicative
import Data.Attoparsec.Text
import System.Exit (exitFailure) 

main :: IO ()
main = do
	{-
	print "parse button element"
	let buttonElement = parseOnly (parseElement "button") "<button width='12' action='sendJson'></button>"
	print buttonElement
	print "parse button element to Button Type"
	let buttonParserResult = parseOnly buttonParser "<button width='12' action='sendJson'></button>"
	print buttonParserResult

	print "genericAttributeDecoder WidthAttribute:" 
	let t = genericAttributeDecoder [Attribute "width" "12"] :: [WidthAttribute]
	print t

	print "genericAttributeDecoder ActionAttribute:"
	let a = genericAttributeDecoder [Attribute "action" "sendJson"] :: [ActionAttribute]
	print a

	print "genericAttributeDecoder MaxAttributeDouble:"
	let db = genericAttributeDecoder [Attribute "maxd" "12"] ::  [MaxAttributeDouble]
	print db
	
	let ll = parseOnly labelParser "<label width='12'>Legal Dest</label>"
	print ll
	-}


	print "genericAttributeDecoder InputTypeAttribute:"
	let inputAt = genericAttributeDecoder [Attribute "type" "double"] :: [InputTypeAttribute]
	print inputAt

	--print "genericAttributeDecoder InputTypeAttribute:"
	--let w = fromAttribute <$> [InputWidth $ Attribute "width" "1.0"]
	--print w
	--print $ parseInputType inputAt

	let x = parseOnly (parseElement "input") "<input type='double' indexable='True'>3.3</input>"
	print x
	case x of
		Right x' -> do 
			let y = (parseInputType (genericAttributeDecoder $ attributes x') (value x'))
			print y
		Left _ -> print "didn't parse"


	
	--print $ Input (parseInputType inputAt "3.3") inputAt
	{-
	let sigTest = parseOnly (parseElement "input") "<input type='signature'>blahblahblah</input>"
	print sigTest
	
	case sigTest of
		Right x' -> do 
			print $ attributes x'
			let as = genericAttributeDecoder (attributes x') :: [InputAttribute]
			print as
			let y = (parseInputType (as) (value x'))
			print y
		Left _ -> print "didn't parse"

	print "parse input with Maxd:"
	let maxdButton = parseOnly (parseElement "input") "<input type='double' maxd='12.3'>3.3</input>"
	print maxdButton
	case maxdButton of
		Right x' -> do 
			let y = (parseInputType (genericAttributeDecoder $ attributes x') (value x'))
			print y
		Left _ -> print "didn't parse"

	let x = parseOnly (parseElement "input") "<input type='double' indexable='True'>3.3</input>"
	print x
	case x of
		Right x' -> do 
			let y = (parseInputType (genericAttributeDecoder $ attributes x') (value x'))
			print y
		Left _ -> print "didn't parse"

	let e = parseOnly (parseElement "input") "<input type='borke' indexable='True'>3.3</input>"
	print e
	case e of
		Right x' -> do 
			let y = (parseInputType (genericAttributeDecoder $ attributes x') (value x'))
			print y
		Left _ -> print "didn't parse"
	-}
	--let z = Attribute "width" "12"
	--print z
	--print $ fromAttribute z
	--print $ genericAttributeDecoder defaultInputAttributesList
	{-
	case c of
		Right c' -> print $ genericAttributeDecoder $ attributes c'
		Left _ -> print "nothing"
	print defaultButtonAttributeList
	-}
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