{-# LANGUAGE OverloadedStrings #-}

module Main where

import Kiosk.Backend.Form

import Control.Applicative
import Data.Attoparsec.Text
import System.Exit (exitFailure) 

-- I found this commented out in the old version
-- not sure if it is still supposed to look like this
-- "<input type='double' indexable='True'>0.0</input>"
-- expectedInputDouble :: Input
-- expectedInputDouble = Input {_getInput = InputTypeText (InputText {_getInputText = ""}), _inputAttrib = [InputType (InputTypeAttribute {_getTypeName = InputTypeDouble (InputDouble {_getInputDouble = 0.0})}),InputIndexable (IndexableAttribute {_getIndexable = True})]}

main :: IO ()
main = do
	-- with indexable
	-- index forces the type to be InputTypeText, value is lost
	print $ parseOnly inputParser "<input type='double' indexable='True'>3.3</input>"
	print $ parseOnly inputParser "<input type='text' indexable='True'>Plowtech</input>"
	print $ parseOnly inputParser "<input type='signature' indexable='True'>as9d8j2l3kfaoiu1239h</input>"
	print $ parseOnly inputParser "<input type='int' indexable='True'>1234</input>"
	
	-- without indexable
	print $ parseOnly inputParser "<input type='double'>3.3</input>"
	print $ parseOnly inputParser "<input type='text'>Plowtech</input>"
	print $ parseOnly inputParser "<input type='signature'>as9d8j2l3kfaoiu1239h</input>"
	print $ parseOnly inputParser "<input type='int'>1234</input>"
	
	-- button and label
	print $ parseOnly buttonParser "<button width='12' action='sendJson'></button>"
	print $ parseOnly labelParser "<label width='12'>Legal Dest</label>"

	exitFailure