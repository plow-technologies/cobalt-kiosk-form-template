{-# LANGUAGE OverloadedStrings #-}

module Kiosk.Backend.Form.Parsers where

import           Kiosk.Backend.Form.Attribute
import           Kiosk.Backend.Form.Attribute.Indexable
import           Kiosk.Backend.Form.Attribute.Path
import           Kiosk.Backend.Form.Attribute.Width
import           Kiosk.Backend.Form.Element

import           Control.Applicative
import           Control.Applicative.Permutation
import           Data.Attoparsec.Text
import           Data.Either

import           Data.List                              ((\\))
import           Data.Monoid                            ((<>))
import           Data.Text                              (Text)
import qualified Data.Text                              as T


parseForm :: Parser Form
parseForm = do
  _ <- parseOpenTag "entry"
  _ <- parseOpenTag "form"
  company <- parseCompanyElement
  address <- parseAddressElement
  logo    <- parseLogoElement
  phone   <- parsePhoneElement
  constants <- many' $ try parseConstantElement
  rows      <- many' $ try parseRow
  parseCloseTag "form"
  parseCloseTag "entry"
  return $ Form company address logo phone constants rows


-- parse path attribute
parseCompanyElement :: Parser Company
parseCompanyElement = do
  company <- parseElementWithoutAttributes "company"
  return $ Company (value company) []

parseAddressElement :: Parser Address
parseAddressElement = do
  address <- parseElementWithoutAttributes "address"
  return $ Address (value address) []

parseLogoElement :: Parser Logo
parseLogoElement = do
  logo <- parseElementWithRequiredAttributes "logo" ["path"]
  let logoAttribs = map (LogoPath . PathAttribute . val ) $ filter (\x -> name x == "path") (attributes logo)
  return $ Logo (value logo) logoAttribs

parsePhoneElement :: Parser Phone
parsePhoneElement = do
  phone <- parseElementWithoutAttributes "phone"
  return $ Phone (value phone) []

parseConstantElement :: Parser Constant
parseConstantElement = do
  constant <- parseElementWithRequiredAttributes "constant" ["type","indexable"]
  return $ Constant (value constant) (map parseConstantAttributeType $ attributes constant)

parseConstantAttributeType :: Attribute -> ConstantAttributes
parseConstantAttributeType (Attribute "type"      v      ) = ConstantAttributeType v
parseConstantAttributeType (Attribute "indexable" "True" ) = ConstantAttributeIndexable $ IndexableAttribute True
parseConstantAttributeType (Attribute "indexable" "False") = ConstantAttributeIndexable $ IndexableAttribute False
parseConstantAttributeType (Attribute _           _      ) = ConstantAttributeType ""

parseRow :: Parser Row
parseRow = parseOpenTag "row" *> (buildRow <$> possibleItems)   <* parseCloseTag "row"
  where
     buildRow = flip Row []
     possibleItems = many.try $ itemParser



-- parseInputOfType :: T.Text -> Parser Item
-- parseInputOfType = undefined
-- parseInputOfType inputType = do
--   -- look for width or break
--   _iElem <- parseOpenTag "item"

--   itemLabel <- labelParser
--   inputElem <- parseElement inputType

--   -- look for width or break
--   let
--       itemLabel = Label (element labelElem) (decodeAttributeList . attributes $ labelElem)
--       itemInput = Input (parseInputType (decodeAttributeList . attributes $ inputElem) (value inputElem))
--                         (decodeAttributeList . attributes $ inputElem)

--   parseCloseTag "item"
--   return $ Item [ItemLabel itemLabel, ItemInput itemInput] [ItemWidth $ WidthAttribute (12::Int)]

itemParser = parseElement "item" itemFromAttrs
  where
   itemFromAttrs _attrs = parseItemInput <|>
                          parseItemButton <|>
                          parseItemRadio <|>
                          parseItemLabel
parseItemInput = do
      itemLabel <- labelParser
      itemInput <- inputParser
      return $ Item [ ItemLabel itemLabel
                    , ItemInput itemInput]
                    [ItemWidth $ WidthAttribute (12::Int)]

  -- $ Item [ItemButton (Button (value buttonElement) (attributes buttonElement))] [ItemWidth $ WidthAttribute (12::Int)]
parseItemButton :: Parser Item
parseItemButton = makeItemButton <$> buttonParser
  where
   makeItemButton b = Item [ItemButton b] [ItemWidth $ WidthAttribute (12::Int)]

parseItemLabel :: Parser Item
parseItemLabel = makeItemLabel <$> labelParser
  where
  makeItemLabel itemLabel = Item [ItemLabel itemLabel] [ItemWidth $ WidthAttribute (12::Int)]


parseItemRadio :: Parser Item
parseItemRadio = undefined -- do
--   _iElem <- parseOpenTag "item" <?> "parseRadio: did not find item."
--   _ <- parseOpenTag "radio" <?> "parseRadio: did not find radio."
-- --  labelElem <- parseElement "label"　<?> "parseRadio: did not find label."
-- --  let itemLabel = Label (element labelElem) (decodeAttributeList $ attributes labelElem)
--   itemLabel <- labelParser
--   optionElements <- many1 $ parseElement "option"
--   --Option "Pit Water" []
--   -- currently not using option attributes
--   let ops = map (\x -> Option (value x ) []) optionElements
--   opqs <- many' parseOptionQualifier
--   _ <- parseCloseTag "radio" <?> "parseRadio: did not find radio close tag."
--   _ <- parseCloseTag "item" <?> "parseRadio: did not find item close tag."
--   return $ Item [ItemRadio $ Radio itemLabel ops opqs] [ItemWidth $ WidthAttribute (12::Int)]
--    where
--     parseOptionQualifier :: Parser OptionQualifier
--     parseOptionQualifier = do
--       _iElem <- parseOpenTag "option-qualifier"
--       labelElem <- parseElement "label"
--       inputElem <- parseElement "input"
--       -- look for width or break
--       let itemLabel = Label (element labelElem) (decodeAttributeList $ attributes labelElem)
--       let itemInput = Input (parseInputType (decodeAttributeList $ attributes inputElem) (value inputElem)) (decodeAttributeList $ attributes inputElem)
--       parseCloseTag "item"
--       return $ OptionQualifier [QualifierLabel itemLabel, QualifierInput itemInput] []









-- | Element with Content Parser


-- Button Parser
buttonParser :: Parser Button
buttonParser = parseElement "button" buttonFromAttrs
     where
       buttonFromAttrs attrs  = flip Button (decodeAttributeList attrs) <$>
                                parseElementBodyAsText

-- Label Parser
labelParser :: Parser Label
labelParser = parseElement "label" makeLabel
    where
      makeLabel attrs =  flip Label (decodeAttributeList attrs) <$>
                         parseElementBodyAsText

inputParser :: Parser Input
inputParser = parseElement "input" inputFromAttrs
     where
       inputFromAttrs attrs = flip Input  (decodeAttributeList attrs) <$>
                              parseInputType (decodeAttributeList attrs)


parseInputType :: [InputAttribute]  -> Parser InputType
parseInputType (InputType InputTypeAttributeInt:_)         = InputTypeInt       . InputInt    <$> signed decimal
parseInputType (InputType InputTypeAttributeDouble:_)      = InputTypeDouble    . InputDouble <$> signed double
parseInputType (InputType InputTypeAttributeText      :_)  = InputTypeText      . InputText   <$> parseElementBodyAsText
parseInputType (InputType InputTypeAttributeSignature:_)   = InputTypeSignature . Signature   <$> parseElementBodyAsText
parseInputType _                                                 = return $ InputTypeText . InputText $ ""



-- | Parse primitives


-- | decode a list of attributes in a given type
-- | just drop attributes that are left instances
decodeAttributeList :: AttributeClass t => [Attribute] -> [t]
decodeAttributeList = rights . fmap fromAttribute

-- | Parse a complete element from opening tag to closing tag

parseElement elemName attrsParser = do
  tag <- parseOpenTag elemName
  rslt <- attrsParser . tagAttrs $ tag
  _ <- parseCloseTag elemName
  return rslt

-- | Parser purposefully disgards any parsed elements
parseElementWithoutAttributes :: T.Text -> Parser Element
parseElementWithoutAttributes elemName = undefined --do
--                         elem <- parseElement elemName
--                         return $ elem {attributes= []}

-- | Parser Fails unless given Attribute text are found
parseElementWithRequiredAttributes :: T.Text -> [T.Text] -> Parser Element
parseElementWithRequiredAttributes elemName requiredAttrs = undefined -- do
  -- elem <- parseElement elemName
  -- if null $ requiredAttrs \\ (fmap name . attributes $ elem)
  --     then
  --        return elem
  --     else
  --        fail   $ T.unpack  $
  --     "parseElementWithRequiredAttributes parsed the following attributes: " <>
  --     T.intercalate ", " (map name (attributes elem)) <>
  --     ", but requires the following attributes: " <>
  --     T.intercalate ", " requiredAttrs <> "."

parseAttributes :: Parser Attribute
parseAttributes = do
  nameRest <- manyTill letter (char '=')
  q <- char '\'' <|> char '"'
  attrVal <- takeTill (== q)
  _ <- char q
  return $ Attribute (T.pack nameRest) attrVal


-- | the combinators below permutate to give any available combination in a tag
-- so < name a1='v1' a2='v2' a3='v3'> val </name>
-- parses the same as < name a3='v3' a2='v2' a1='v1'> val </name>
-- and so on... this is important because many xml libs don't respect attr order
-- A sumtype and list is used to create this parsing style then at the end the
-- the element record is constructed from the list
-- lastly, the element that is created is validated

parseClosingAngle :: Parser Char
parseClosingAngle = tokenChar '>'
                    <?> "Did not find closing angle '>'"


parseOpeningAngle :: Parser Char
parseOpeningAngle = tokenChar '<'
                    <?> "parseOpenTag did not find opening angle '<'"



-- > mainParser  = sum <$ whiteSpace <*> many (token digit) <* eof
-- | separates each part of the above into a sum type for alternative parser|

parseElementBodyAsText = takeTill (== '<')

parseOpenTag :: Text -> Parser Tag
parseOpenTag tname = angles (parseTagParts tname)
 where
  parseTagParts :: Text -> Parser Tag
  parseTagParts tname' = runPerms $ Tag <$>
                          atom (parseTagName tname') <*>
                          atom parseAttributeToken

parseCloseTag :: Text -> Parser ()
parseCloseTag tname = (flip Tag []  <$>
                      closingAngles (parseTagName tname)) >> return ()

parseTagName :: Text -> Parser Text
parseTagName tname = token  (string tname) <?>
                    "parseTagName did not find '" <> T.unpack tname <> "'"

parseAttributeToken :: Parser [Attribute]
parseAttributeToken = many' (token parseAttributes) <?>
                     "parseAttribute caused an error"

angles :: Parser middle -> Parser middle
angles = between (tokenChar '<') (tokenString ">")

closingAngles :: Parser middle -> Parser middle
closingAngles = between (tokenString "</") (tokenString ">")

between :: Parser leftBracket
        -> Parser rightBracket
        -> Parser middle -> Parser middle
between ip fp middleP = ip *> middleP <* fp

tokenChar :: Char -> Parser Char
tokenChar c = token (char c)

tokenString :: Text -> Parser Text
tokenString s = token (string s)

token :: Parser a -> Parser a
token a = a <* (someSpace <|> pure ())

someSpace :: Parser ()
someSpace = skipMany1 space



-- |Types for parse assistant

{-|
a Tag is <tag>

an element is (<tag> element body </tag>

|-}

data Element = Element {
    element    :: T.Text
  , attributes :: [Attribute]
  , value      :: T.Text
} deriving (Show)

data Tag = Tag { tagName  :: Text
               , tagAttrs :: [Attribute] }
 deriving (Show)
