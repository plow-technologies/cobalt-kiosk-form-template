{- |
Module      :  Kiosk.Backend.Parsers
Description :  XML Parsers for the Forms
Copyright   :  (c) <Plow Technology 2014>
License     :  <MIT>
Maintainer  :  <lingpo.huang@plowtech.net>
Stability   :  unstable
Portability :  portable
<Parse the XML coming from frontend into Haskell Types>
-}

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}

module Kiosk.Backend.Form.Parsers where

import           Data.Either                  (rights)
import           Data.Either.Validation       (validationToEither)
import           Data.Text                    (pack)
import           Kiosk.Backend.Form.Attribute
import           Kiosk.Backend.Form.Element
import           Text.Trifecta

import           Control.Applicative          ((<$>), (<|>))

data Element = Element { element    :: String
                        ,attributes :: [Attribute]
                        ,value      :: String
                        }

  deriving (Show)

        -- do
        -- let eAttrList = validationToEither. fromAttribute <$> attr
        -- case rights eAttrList of
        --   [] -> if null.lefts $ eAttrList  -- If there are no good, but still bad we want fail
        --         then return []
        --         else fail "Invalid Button Attribute"
        --   attrs -> return attrs

testText :: String
testText = "<element attr1='3' attr2='Cat'> Some Text </element>"

testButton :: String
testButton = "<button action='sendJson'>Send JSON </button>"

testLabel :: String
testLabel = "<label>Time Loaded</label>"

testInput :: String
testInput = "<input type='text'>Send JSON</input>"


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

buttonAttrFromAtt :: AttributeClass t => [Attribute] -> [t]
buttonAttrFromAtt attr = do
                      let eAttrList = validationToEither. fromAttribute <$> attr
                      case rights eAttrList of
                           [] -> []
                           attrs -> attrs

parseInputType :: [InputAttribute] -> String -> InputType
parseInputType ([InputType (InputTypeAttribute (InputTypeText _) )]) elemVal =  InputTypeText . InputText . pack $ elemVal
parseInputType ([InputType (InputTypeAttribute (InputTypeSignature _))]) elemVal = InputTypeSignature. Signature . pack $ elemVal
parseInputType ([InputType (InputTypeAttribute (InputTypeInt _))]) elemVal = InputTypeInt . InputInt $ (read elemVal :: Int)
parseInputType ([InputType (InputTypeAttribute (InputTypeDouble _))]) elemVal = InputTypeDouble . InputDouble $ (read elemVal :: Double)
parseInputType [] _ = InputTypeText. InputText . pack $ ""
parseInputType _  _ = InputTypeText. InputText . pack $ ""


-- | Element with Content Parser

-- Button Parser
buttonParser :: (TokenParsing f, Monad f) => f Button
buttonParser = buttonFromElement <$> parseElement "button"
    where
      buttonFromElement (Element _ attr elemVal) = Button (pack elemVal) (buttonAttrFromAtt attr)

-- Label Parser
labelParser :: (TokenParsing f, Monad f) => f Label
labelParser = buttonFromElement <$> parseElement "label"
    where
      buttonFromElement (Element _ attr elemVal) = Label (pack elemVal) (buttonAttrFromAtt attr)

inputParser :: (TokenParsing f, Monad f) => f Input
inputParser = buttonFromElement <$> parseElement "input"
    where
      buttonFromElement (Element _ attr elemVal) = Input (parseInputType (buttonAttrFromAtt attr) elemVal) (buttonAttrFromAtt attr)


