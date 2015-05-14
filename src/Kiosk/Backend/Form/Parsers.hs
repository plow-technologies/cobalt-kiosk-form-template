{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}

module Kiosk.Backend.Form.Parsers where

import           Kiosk.Backend.Form.Attribute
import           Kiosk.Backend.Form.Element

import           Control.Applicative

import           Data.Attoparsec.Text
import           Data.Either
import qualified Data.Text                    as T


data Element = Element {
    element    :: T.Text
  , attributes :: [Attribute]
  , value      :: T.Text
} deriving (Show)


textOrNullParser :: Parser String
textOrNullParser = manyTill anyChar (char '<')

-- Generic Element Parser

parseElement :: T.Text -> Parser Element
parseElement elemName = do
  _ <- char '<' <?> "Didn not find opening angle '<'"

  _ <- string elemName  <?> "Didn't find elemName tag"
  attrList <- try $ many' parseAttributes
  _ <- char '>' <?> "Did not find closing angle '>'"

  elemValue <- textOrNullParser

  return $ Element elemName attrList (T.pack elemValue)

-- break on empty attrName
-- make sure length attrName' == attrName
parseAttributes :: Parser Attribute
parseAttributes = do
  _ <- many1 space
  
  nameFirstLetter <- letter
  nameRest <- manyTill letter (char '=')
  
  q <- char '\'' <|> char '"'
  attrVal <- takeTill (== q)
  _ <- char q

  return $ Attribute (T.pack $ nameFirstLetter:nameRest) attrVal

genericAttributeDecoder :: AttributeClass t => [Attribute] -> [t]
genericAttributeDecoder attrs = do
  let eAttrList = fromAttribute <$> attrs
  case rights eAttrList of
    [] -> []
    attrs' -> attrs'

parseInputType :: [InputAttribute] -> T.Text -> InputType
parseInputType ([InputType (InputTypeAttributeText )])      elemVal = InputTypeText      . InputText $ elemVal
parseInputType ([InputType (InputTypeAttributeSignature )]) elemVal = InputTypeSignature . Signature $ elemVal
parseInputType ([InputType (InputTypeAttributeInt )])       elemVal = InputTypeInt       . InputInt    $ (read (T.unpack elemVal) :: Int)
parseInputType ([InputType (InputTypeAttributeDouble)])     elemVal = InputTypeDouble    . InputDouble $ (read (T.unpack elemVal) :: Double)
parseInputType [] _ = InputTypeText . InputText $ ""
parseInputType _  _ = InputTypeText . InputText $ ""

-- | Element with Content Parser

-- Button Parser
buttonParser :: Parser Button
buttonParser = buttonFromElement <$> parseElement "button"
    where
      buttonFromElement (Element _ attrs elemVal) = Button elemVal (genericAttributeDecoder attrs)

-- Label Parser
labelParser :: Parser Label
labelParser = labelFromElement <$> parseElement "label"
    where
      labelFromElement (Element _ attrs elemVal) = Label elemVal (genericAttributeDecoder attrs)

inputParser :: Parser Input
inputParser = inputFromElement <$> parseElement "input"
    where
      inputFromElement (Element _ attrs elemVal) = Input (parseInputType (genericAttributeDecoder attrs) elemVal) (genericAttributeDecoder attrs)
      


