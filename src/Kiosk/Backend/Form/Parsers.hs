{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}

module Kiosk.Backend.Form.Parsers where

import           Kiosk.Backend.Form.Attribute
import           Kiosk.Backend.Form.Attribute.Width
import           Kiosk.Backend.Form.Element


import           Control.Applicative

import           Data.Attoparsec.Text
import           Data.Either
import           Data.List (sort)
import qualified Data.Text                    as T


data Element = Element {
    element    :: T.Text
  , attributes :: [Attribute]
  , value      :: T.Text
} deriving (Show)

{-
form
 logo
 company
 phone
 address
 constant

 row
   item
     label
     input
 row
   item
     label

 row
  item
    radio
      label
      option
      option-qualifier
        label
        input
  row
    item
      signature
-}

parseForm :: Parser Bool
parseForm = do 
  parseOpenTag "entry"
  parseOpenTag "form"
  
  _ <- many' $ try $ parseCompanyElement <|> parseAddressElement

  _ <- many' $ try $ parseRow

  parseCloseTag "form"
  parseCloseTag "entry"
  return True

{-
parseFormContents :: Parser Bool
-}

-- row
--   item
--   item
-- company
-- row
-- [row [item, item]]

parseOpenTag :: T.Text -> Parser ()
parseOpenTag elemName = do
  _ <- char '<' <?> "parseOpenTag did not find opening angle '<'"
  _ <- string elemName  <?> "parseOpenTag did not find elemName tag"
  _ <- char '>' <?> "parseOpenTag did not find closing angle '>'"
  return ()

parseOpenTagWithAttributes :: T.Text -> Parser Element
parseOpenTagWithAttributes elemName = do
  _ <- char '<' <?> "parseOpenTag did not find opening angle '<'"
  _ <- string elemName  <?> "parseOpenTag did not find elemName tag"
  attrList <- try $ many' parseAttributes
  _ <- char '>' <?> "parseOpenTag did not find closing angle '>'"
  return $ Element elemName attrList ""

parseCloseTag :: T.Text -> Parser ()
parseCloseTag elemName = do
  _ <- char '<' <?> "parseCloseTag did not find opening angle '<'"
  _ <- char '/' <?> "parseCloseTag did not find backslash '/'"
  _ <- string elemName  <?> "parseCloseTag did not find elemName tag"
  _ <- char '>' <?> "parseCloseTag did not find closing angle '>'"
  return ()


-- parse path attribute
parseLogoElement :: Parser Element
parseLogoElement = parseElementWithRequiredAttributes "logo" ["path"]

parseCompanyElement :: Parser Element
parseCompanyElement = parseElementWithoutAttributes "company"

parsePhoneElement :: Parser Element
parsePhoneElement = parseElementWithoutAttributes "phone"

parseAddressElement :: Parser Element
parseAddressElement = parseElementWithoutAttributes "address"

parseConstantElement :: Parser Element
parseConstantElement = parseElementWithRequiredAttributes "constant" ["type"]

--parseConstantElement :: Parser Element
--parseConstantElement = -- parse with attribute type

parseRow :: Parser [Item]
parseRow = do 
  parseOpenTag "row"

  items <- many $ try $ parseGeneralInput <|> parseSign
  --parseOpenTag "item"

  --_ <- many $ try $ parseInput <|> parseSignature <|> parseButton <|> parseLabel
  --_ <- many $ try $ parseSignature <|> parseButton <|> parseLabel
  
  --parseCloseTag "item"
  parseCloseTag "row"

  return items

--Item [ItemLabel defaultLabel, ItemInput defaultInput] [ItemWidth $ WidthAttribute (12::Int)]
-- ITEM
-- defaultLabel :: Label
-- defaultLabel = Label "Legal Dest" [LabelWidth $ WidthAttribute (12::Int)]
--defaultInput :: Input
--defaultInput = Input defaultInputType defaultInputAttributesList

--defaultInputType :: InputType
--defaultInputType = InputTypeText $ InputText ("" :: T.Text)

{-
inputParser :: Parser Input
inputParser = inputFromElement <$> parseElement "input"
    where
      inputFromElement (Element _ attrs elemVal) = Input (parseInputType (genericAttributeDecoder attrs) elemVal) (genericAttributeDecoder attrs)
-}

-- inputFromElement
-- getInputType = (Element _ attrs elemVal) = Input (parseInputType (genericAttributeDecoder attrs) elemVal) (genericAttributeDecoder attrs)

parseInput :: T.Text -> Parser Item
parseInput inputType = do
  -- look for width or break
  iElem <- parseOpenTagWithAttributes "item"
  
  labelElem <- parseElement "label"　
  inputElem <- parseElement inputType
  
  -- look for width or break
  let itemLabel = Label (element labelElem) [LabelWidth $ WidthAttribute (12::Int)]
  let itemInput  =  Input (parseInputType (genericAttributeDecoder $ attributes inputElem) (value inputElem)) (genericAttributeDecoder $ attributes inputElem)
  
  parseCloseTag "item"
  return $ Item [ItemLabel itemLabel, ItemInput itemInput] [ItemWidth $ WidthAttribute (12::Int)]

parseGeneralInput :: Parser Item
parseGeneralInput = parseInput "input"

parseSign :: Parser Item
parseSign = parseInput "input"

{-
parseInput :: Parser Item
parseInput = do
  -- look for width or break
  iElem <- parseOpenTagWithAttributes "item"
  
  labelElem <- parseElement "label"　
  inputElem <- parseElement "input"
  
  -- look for width or break
  let itemLabel = Label (element labelElem) [LabelWidth $ WidthAttribute (12::Int)]
  let itemInput  =  Input (parseInputType (genericAttributeDecoder $ attributes inputElem) (value inputElem)) (genericAttributeDecoder $ attributes inputElem)
  
  parseCloseTag "item"
  return $ Item [ItemLabel itemLabel, ItemInput itemInput] [ItemWidth $ WidthAttribute (12::Int)]

parseSignature :: Parser Item
parseSignature = do
  -- look for width or break
  iElem <- parseOpenTagWithAttributes "item"
  
  labelElem <- parseElement "label"　
  signElem <- parseElement "signature"
  
  -- look for width or break
  let itemLabel = Label (element labelElem) [LabelWidth $ WidthAttribute (12::Int)]
  let itemInput  =  Input (parseInputType (genericAttributeDecoder $ attributes inputElem) (value inputElem)) (genericAttributeDecoder $ attributes inputElem)
  
  parseCloseTag "item"
  return $ Item [ItemLabel itemLabel, ItemInput itemInput] [ItemWidth $ WidthAttribute (12::Int)]
-}

parseSignature :: Parser ()
parseSignature = do
  parseElement "signature"
  return ()

-- should have width and action
parseButton :: Parser ()
parseButton = do
  parseElement "button"
  return ()

parseLabel :: Parser ()
parseLabel = do
  parseElement "label"
  return ()

parseRadio :: Parser ()
parseRadio = do
  parseOpenTag "radio"
  parseElement "option"
  
  {- optional
  parseOpenTag "option-qualifier"
  parseCloseTag "option-qualifier"
  -}
  parseCloseTag "radio"
  return ()

{-
radio
      label
      option
      option-qualifier
        label
        input
-}


-- zip fold elem required attr, found attrs
-- map (map elem [1,2,3]) [1,2,3]
-- map (map (+) [1,2,3]) [1,2,3]
-- map (\x -> elem x)
textOrNullParser :: Parser T.Text
--textOrNullParser = manyTill' anyChar (char '<')
textOrNullParser = takeTill (== '<')
-- Generic Element Parser

parseElementWithoutAttributes :: T.Text -> Parser Element
parseElementWithoutAttributes elemName = do
  _ <- char '<' <?> "Did not find opening angle '<'"

  _ <- string elemName  <?> "parseElement did not find elemName tag"
  _ <- char '>' <?> "Did not find closing angle '>'"

  elemValue <- textOrNullParser
  _ <- parseCloseTag elemName

  return $ Element elemName [] elemValue


-- parseElementWithOptionalAttributes
-- the attributes might exist

parseElementWithRequiredAttributes :: T.Text -> [T.Text] -> Parser Element
parseElementWithRequiredAttributes elemName requiredAttrs = do
  _ <- char '<' <?> "Did not find opening angle '<'"

  _ <- string elemName  <?> "parseElement did not find elemName tag"
  attrList <- try $ many' parseAttributes
  _ <- char '>' <?> "Did not find closing angle '>'"

  elemValue <- textOrNullParser
  _ <- parseCloseTag elemName

  case (sort requiredAttrs) == (sort (map name attrList)) of
    True  -> return $ Element elemName attrList elemValue
    False -> fail   $ T.unpack  $ T.concat $ 
      ["parseElementWithRequiredAttributes parsed the following attributes: "] ++ [(T.intercalate ", " (map name attrList))] ++
      [", but requires the following attributes: "] ++ [(T.intercalate ", " requiredAttrs)] ++ ["."]
    {-
    False -> fail "parseElementWithRequiredAttributes"
    False -> fail   $ T.unpack  $ T.concat $ 
      ["parseElementWithRequiredAttributes parsed the following attributes: "] ++ [(T.intercalate ", " (map name attrList))] ++
      [", but requires the following attributes: "] ++ [(T.intercalate ", " requiredAttrs)] ++ "."
    -}
parseElement :: T.Text -> Parser Element
parseElement elemName = do
  _ <- char '<' <?> "Did not find opening angle '<'"

  _ <- string elemName  <?> "parseElement did not find elemName tag"
  attrList <- try $ many' parseAttributes
  _ <- char '>' <?> "Did not find closing angle '>'"

  elemValue <- textOrNullParser
  _ <- parseCloseTag elemName

  return $ Element elemName attrList elemValue
  
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
parseInputType (InputType (InputTypeAttributeText      ):_) elemVal = InputTypeText      . InputText   $ elemVal
parseInputType (InputType (InputTypeAttributeSignature ):_) elemVal = InputTypeSignature . Signature   $ elemVal
parseInputType (InputType (InputTypeAttributeInt       ):_) elemVal = InputTypeInt       . InputInt    $ (read (T.unpack elemVal) :: Int)
parseInputType (InputType (InputTypeAttributeDouble    ):_) elemVal = InputTypeDouble    . InputDouble $ (read (T.unpack elemVal) :: Double)
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
      


