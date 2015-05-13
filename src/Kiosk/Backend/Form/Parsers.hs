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


-- Generic Element Parser

{-
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


parseElementZ :: String -> Parser Element
parseElementZ nameString = innerElementParser
 where
   textOrNullParser = many $ noneOf "<"
   innerElementParser = do
             (elemName, attrList) <- angles parseElement'
             elemValue <- textOrNullParser
             _ <- angles $ many (noneOf ">")
             return $ Element elemName attrList elemValue
   parseElement' = do _ <- symbol nameString
                      attrList <- many parseAttributes
                      return (nameString, attrList)
-}

textOrNullParser :: Parser String
textOrNullParser = manyTill anyChar (char '<') -- string "<!--" *> manyTill anyChar (string "-->") -- manyTill string "<"

parseElement :: T.Text -> Parser Element
parseElement elemName = do
  _ <- char '<' <?> "Didn not find opening angle '<'"
  --closingTag <- optional $ char '/'

  _ <- string elemName  <?> "Didn't find elemName tag"
  attrList <- try $ many' parseAttributes
  _ <- char '>' <?> "Did not find closing angle '>'"

  --elemValue <- letter *> manyTill char '<'
  elemValue <- textOrNullParser
  --emptyTag <- optional $ char '/'
  {-
  let t = (if   isJust closingTag 
         then ClosingTag
         else if isJust emptyTag   
            then EmptyTag
            else OpeningTag)
  -}
  return $ Element elemName attrList (T.pack elemValue)

-- break on empty attrName
-- make sure length attrName' == attrName
parseAttributes :: Parser Attribute
parseAttributes = do
  _ <- many1 space
  
  --_ <- string attrName <?> "Attribute name string did not match"
  --_ <- char '='  <?> "Attribute name string did not match"
  nameFirstLetter <- letter
  nameRest <- manyTill letter (char '=')
  
  q <- char '\'' <|> char '"'
  attrVal <- takeTill (== q)
  _ <- char q

  let a = Attribute (T.pack $ nameFirstLetter:nameRest) attrVal
  
  return a

genericAttributeDecoder :: AttributeClass t => [Attribute] -> [t]
genericAttributeDecoder attrs = do
  let eAttrList = fromAttribute <$> attrs
  case rights eAttrList of
    [] -> []
    attrs' -> attrs'
-- genericAttributeDecoder :: AttributeClass t => [Attribute] -> [Parser t]

{-
genericAttributeDecoder :: AttributeClass t => [Attribute] -> [t]
genericAttributeDecoder  attr = do
                       let eAttrList = validationToEither. fromAttribute <$> attr
                       case rights eAttrList of
                            [] -> []
                            attrs -> attrs
-}
{-
stringLiteral :: Parser Text

-- Generic Element Parser

parseElement :: (TokenParsing m, Monad m) => String -> m Element
parseElement nameString = innerElementParser
 where
   textOrNullParser = many $ noneOf "<"
   innerElementParser = do
             (elemName, attrList) <- angles parseElement'
             elemValue <- textOrNullParser
             _ <- angles $ many (noneOf ">")
             return $ Element elemName attrList elemValue
   parseElement' = do _ <- symbol nameString
                      attrList <- many parseAttributes
                      return (nameString, attrList)

-- Generic Attribute Parser
parseAttributes :: (TokenParsing m, Monad m) => m Attribute
parseAttributes = do spaces
                     firstletter <- letter
                     rest <- manyTill alphaNum (char '=')
                     literal <- stringLiteral <|> stringLiteral'
                     return $ Attribute (pack $ firstletter:rest) (pack literal)

genericAttributeDecoder :: AttributeClass t => [Attribute] -> [t]
genericAttributeDecoder  attr = do
                       let eAttrList = validationToEither. fromAttribute <$> attr
                       case rights eAttrList of
                            [] -> []
                            attrs -> attrs
-}

parseInputType :: [InputAttribute] -> T.Text -> InputType
parseInputType ([InputType (InputTypeAttributeText )]) elemVal =  InputTypeText . InputText . T.pack $ (T.unpack elemVal)
parseInputType ([InputType (InputTypeAttributeSignature )]) elemVal = InputTypeSignature. Signature . T.pack $ (T.unpack elemVal)
parseInputType ([InputType (InputTypeAttributeInt )]) elemVal = InputTypeInt . InputInt $ (read (T.unpack elemVal) :: Int)
parseInputType ([InputType (InputTypeAttributeDouble )]) elemVal = InputTypeDouble . InputDouble $ (read (T.unpack elemVal) :: Double)
parseInputType [] _ = InputTypeText. InputText . T.pack $ ""
parseInputType _  _ = InputTypeText. InputText . T.pack $ ""


-- | Element with Content Parser

-- Button Parser
buttonParser :: Parser Button
buttonParser = buttonFromElement <$> parseElement "button"
    where
      buttonFromElement (Element _ attr elemVal) = Button elemVal (genericAttributeDecoder attr)

-- Label Parser
labelParser :: Parser Label
labelParser = labelFromElement <$> parseElement "label"
    where
      labelFromElement (Element _ attr elemVal) = Label elemVal (genericAttributeDecoder attr)

inputParser :: Parser Input
inputParser = inputFromElement <$> parseElement "input"
    where
      inputFromElement (Element _ attr elemVal) = Input (parseInputType (genericAttributeDecoder attr) elemVal) (genericAttributeDecoder attr)
      


