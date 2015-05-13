{-# LANGUAGE OverloadedStrings #-}


module Kiosk.Backend.Form (module Kiosk.Backend.Form) where

import Kiosk.Backend.Form.Attribute           as Kiosk.Backend.Form
import Kiosk.Backend.Form.Attribute.Action    as Kiosk.Backend.Form
import Kiosk.Backend.Form.Attribute.Indexable as Kiosk.Backend.Form
import Kiosk.Backend.Form.Attribute.Max       as Kiosk.Backend.Form
import Kiosk.Backend.Form.Attribute.Min       as Kiosk.Backend.Form
import Kiosk.Backend.Form.Attribute.Path      as Kiosk.Backend.Form
import Kiosk.Backend.Form.Attribute.Width     as Kiosk.Backend.Form

import Kiosk.Backend.Form.Element             as Kiosk.Backend.Form
 
import Kiosk.Backend.Form.Parsers             as Kiosk.Backend.Form
 

{-
import Control.Applicative

import Data.Attoparsec.Text
import Data.Maybe (isJust)
import qualified Data.Text as T

data TagType = OpeningTag | ClosingTag | EmptyTag deriving (Enum, Show)

isOpeningTag :: TagType -> Bool
isOpeningTag OpeningTag = True
isOpeningTag _ = False

isClosingTag :: TagType -> Bool
isClosingTag ClosingTag = True
isClosingTag _          = False

isEmptyTag :: TagType -> Bool
isEmptyTag EmptyTag = True
isEmptyTag _        = False

data Tag = Tag {
	tagName       :: T.Text,
	tagType       :: TagType, 
	tagAttributes :: [Attribute]	
} deriving (Show)

{-
data Element = Element {
	tag        :: T.Text,
	attributes :: [Attribute],
	content    :: Maybe T.Text
} deriving (Show)
-}
tagsAreBalanced :: [Tag] -> [Tag] -> Bool -> Bool
tagsAreBalanced []     [y] _ = if isClosingTag (tagType y)
										 then False 
									  else if isOpeningTag (tagType y)
									     then False
									  else True

tagsAreBalanced []     (y:ys) _ = if isClosingTag (tagType y)
										 then False 
									  else if isOpeningTag (tagType y)
									     then tagsAreBalanced [y] ys True
									  else tagsAreBalanced [y] ys True


tagsAreBalanced [_] [] _ = False 								  
-- last amount
tagsAreBalanced [x] [y] t = if isClosingTag (tagType y)
								   then if (tagName x == tagName y)
										then (t && True)
										 -- Closing Tag doesn't match top of stack		
								        else False
								else if isOpeningTag (tagType y)
									 then False
									 -- EmptyTag, throw it away
									 else t

-- more closing tags than opening tags
tagsAreBalanced [x] (y:ys) t = if isClosingTag (tagType y )
										 then if (tagName x == tagName y)
										 	then tagsAreBalanced [] ys (t && True)
										 -- Closing Tag doesn't match top of stack		
										 else False
									  else if isOpeningTag (tagType y)
									  	 then tagsAreBalanced [y,x] ys t
									  	 -- EmptyTag, throw it away
									  	 else tagsAreBalanced [x] ys t
-- more opening tags then closing tags if y is not EmptyTag
tagsAreBalanced (_:_) [_] _ = False

tagsAreBalanced (x:xs) (y:ys) t = if isClosingTag (tagType y )
										 then 
										 	if (tagName x == tagName y)
										 	then tagsAreBalanced xs ys (t && True)
										 	-- Closing Tag doesn't match top of stack		
										 	else False
									  else 
									  	if isOpeningTag (tagType y)
									  		then tagsAreBalanced (y:x:xs) ys t
									  		-- EmptyTag, throw it away
									  		else tagsAreBalanced (x:xs) ys t
tagsAreBalanced [] [] t = t


parseAttributes :: Parser Attribute
parseAttributes = do
	_ <- many1 space
	
	nameFirstLetter <- letter
	nameRest <- manyTill letter (char '=')
	
	q <- char '\'' <|> char '"'
	attrVal <- takeTill (== q)
	_ <- char q

	let a = Attribute (T.pack $ nameFirstLetter:nameRest) attrVal
	
	return a

parseTag :: Parser Tag
parseTag = do
	_ <- char '<'
	closingTag <- optional $ char '/'

	c <- many1 letter
	as <- try $ many' parseAttributes

	emptyTag <- optional $ char '/'

	let t = (if   isJust closingTag 
		     then ClosingTag
		     else if isJust emptyTag   
		     	  then EmptyTag
		     	  else OpeningTag)

	_ <- char '>'
	return $ Tag (T.pack c) t as

parseXML :: Parser ([Tag], Bool)
parseXML = do
	tags <- many1 (skipSpace *> parseTag)
	let v = tagsAreBalanced [] tags True

	skipSpace
	return (tags,v)
-}