{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}

module Kiosk.Backend.Form.Element.Item.Input ( Input(..)
                                             , InputType(..)
                                             , InputDouble (..)
                                             , InputInt (..)
                                             , Signature (..)
                                             , InputText(..)
                                             , InputTextMultiLine(..)
                                             , InputDate(..)
                                             , InputTime(..)
                                             , InputAttribute(..)
                                             , InputTypeAttribute(..)
                                             , makeInputDate
                                             , defaultInput
                                             , defaultInputType
                                             , csvDateStd
                                             , defaultInputAttributesList) where
import           Kiosk.Backend.Form.Attribute           (Attribute (..),
                                                         AttributeClass (..),
                                                         wrongAttrResponse)
import           Kiosk.Backend.Form.Attribute.Indexable
import           Kiosk.Backend.Form.Attribute.Max
import           Kiosk.Backend.Form.Attribute.Min
import           Kiosk.Backend.Form.Attribute.Required
import           Kiosk.Backend.Form.Attribute.Width

import           Control.Applicative
import           Data.Attoparsec.Text
import           Data.Typeable                          (Typeable)
import           GHC.Generics                           (Generic)

import           Data.Aeson                             (FromJSON, ToJSON)
import qualified Data.Text                              as T
import           Text.Read                              (readMaybe)

-- Input Type
data Input = Input {
  _getInput    :: InputType,
  _inputAttrib :: [InputAttribute]
} deriving (Generic, Show, Ord, Eq, Typeable)


instance ToJSON Input where
instance FromJSON Input where

-- Input type can be Text input or Signature input
data InputType =  InputTypeText InputText
                | InputTypeTextMultiLine InputTextMultiLine
                | InputTypeSignature Signature
                | InputTypeInt InputInt
                | InputTypeDate InputDate
                | InputTypeTime InputTime
                | InputTypeDouble InputDouble deriving (Generic, Show, Ord, Eq, Typeable)

instance ToJSON InputType where
instance FromJSON InputType where
-- Text Type Input
newtype InputText = InputText {
    _getInputText :: T.Text
} deriving (Generic, Show, Ord, Eq, Typeable)


instance ToJSON InputText where
instance FromJSON InputText where


newtype InputTextMultiLine = InputTextMultiLine {
    _getInputTextMultiLine :: T.Text
} deriving (Generic, Show, Ord, Eq, Typeable)


instance ToJSON InputTextMultiLine where
instance FromJSON InputTextMultiLine where


-- Date Type :Just Text
newtype InputDate = InputDate {
    _getInputDate :: T.Text
} deriving (Generic, Show, Ord, Eq, Typeable)


instance ToJSON InputDate where
instance FromJSON InputDate where

makeInputDate :: T.Text -> Either T.Text InputDate
makeInputDate itxt = InputDate <$> checkDateFormat itxt

csvDateStd :: String
csvDateStd = "%Y/%m/%d"

checkDateFormat :: T.Text -> Either T.Text T.Text
checkDateFormat itxt = case parseOnly testParser itxt of
                            Right _ -> Right itxt
                            Left  _ -> Left  "Failed"



testParser :: Parser ()
testParser = (decimal :: Parser Int)  *>
             char '/' *>
             (decimal :: Parser Int)  *>
             char '/' *>
             (decimal :: Parser Int)  *> endOfInput

-- Time Type :Just Text
newtype InputTime = InputTime {
    _getInputTime :: T.Text
} deriving (Generic, Show, Ord, Eq, Typeable)


instance ToJSON InputTime where
instance FromJSON InputTime where

-- Signature Type Input store as base64 encode Bytestring
newtype Signature = Signature {
  _signature :: T.Text
} deriving (Generic, Show, Ord, Eq, Typeable)


instance ToJSON Signature where
instance FromJSON Signature where

-- Number Type Input
newtype InputInt = InputInt {
    _getInputInt::Int
} deriving (Generic, Show, Ord, Eq, Typeable)

instance ToJSON InputInt where
instance FromJSON InputInt where

-- Number Type Double
newtype InputDouble = InputDouble {
    _getInputDouble::Double
} deriving (Generic, Show, Ord, Eq, Typeable)


instance ToJSON InputDouble where
instance FromJSON InputDouble where

-- Input Attributes
data InputAttribute = InputWidth WidthAttribute
                    | InputType InputTypeAttribute
                    | InputIndexable IndexableAttribute
                    | InputMaxDouble MaxAttributeDouble
                    | InputMinDouble MinAttributeDouble
                    | InputRequired  RequiredAttribute
                   deriving (Generic, Show, Ord, Eq, Typeable)


instance ToJSON InputAttribute where
instance FromJSON InputAttribute where

instance AttributeClass InputAttribute where
  toAttribute (InputWidth a) = toAttribute a
  toAttribute (InputIndexable a) = toAttribute a
  toAttribute (InputMaxDouble d) = toAttribute d
  toAttribute (InputMinDouble d) = toAttribute d
  toAttribute (InputRequired  a) = toAttribute a
  toAttribute (InputType i) = toAttribute i


  fromAttribute (Attribute "type" v) = case fromAttribute (Attribute "type" v) of
                                          -- The InputType $ t force the value of v to be InputTypeAttribute
                                          -- Is there a more explicit way to coerce the type?
                                          -- fromAttribute (Attribute "type" v) :: InputTypeAttribute doesn't seem to work
                                          Right t -> Right $ InputType $ t
                                          Left _ ->  Left  $ T.concat ["TypeAttribute value not parsing -->",v]

  fromAttribute (Attribute t v) = case t of
                                      "width" -> case readMaybe (T.unpack v) of
                                                      (Just v') -> Right $ InputWidth $ WidthAttribute v'
                                                      Nothing   -> Left $ T.concat ["WidthAttribute value not parsing -->",t,v]
                                      "maxd" -> case (readMaybe (T.unpack v)) of
                                                     (Just v') -> Right  $ InputMaxDouble $ MaxAttributeDouble v'
                                                     Nothing   -> Left $ T.concat ["MaxAttributeDouble value not parsing -->",t,v]
                                      "mind" -> case (readMaybe (T.unpack v)) of
                                                     (Just v') -> Right  $ InputMinDouble $ MinAttributeDouble v'
                                                     Nothing   -> Left $ T.concat ["MinAttributeDouble value not parsing -->",t,v]
                                      "indexable" -> case (readMaybe (T.unpack v)) of
                                                     (Just v') -> Right  $ InputIndexable $ IndexableAttribute v'
                                                     Nothing   -> Left $ T.concat ["IndexableAttribute value not parsing -->",t,v]
                                      "required" -> case (readMaybe (T.unpack v)) of
                                                     (Just v') -> Right  $ InputRequired $ RequiredAttribute v'
                                                     Nothing   -> Left $ T.concat ["RequiredAttribute value not parsing -->",t,v]
                                      _ -> Left $ T.concat ["TypeAttribute value not parsing -->",t,v]

-- Type Attribute
data InputTypeAttribute = InputTypeAttributeDouble
                        | InputTypeAttributeInt
                        | InputTypeAttributeText
                        | InputTypeAttributeTextMultiLine
                        | InputTypeAttributeSignature
                        | InputTypeAttributeDate
                        | InputTypeAttributeTime
   deriving (Generic, Show, Ord, Eq, Typeable)


instance ToJSON InputTypeAttribute where
instance FromJSON InputTypeAttribute where

-- instance AttributeClass InputType where

instance AttributeClass InputTypeAttribute where
  toAttribute InputTypeAttributeText  = Attribute "type" "'text'"
  toAttribute InputTypeAttributeTextMultiLine  = Attribute "type" "'text-multiline'"
  toAttribute InputTypeAttributeSignature  = Attribute "type" "'signature'"
  toAttribute InputTypeAttributeInt  = Attribute "type" "'int'"
  toAttribute InputTypeAttributeDouble  = Attribute "type" "'double'"
  toAttribute InputTypeAttributeDate  = Attribute "type" "'date'"
  toAttribute InputTypeAttributeTime = Attribute "type" "'time'"
  fromAttribute (Attribute "type" v) = case v of
                                         "text"      -> Right $ InputTypeAttributeText
                                         "text-multiline" -> Right $ InputTypeAttributeTextMultiLine
                                         "signature" -> Right $ InputTypeAttributeSignature
                                         "int"       -> Right $ InputTypeAttributeInt
                                         "double"    -> Right $ InputTypeAttributeDouble
                                         "date"      -> Right $ InputTypeAttributeDate
                                         "time"      -> Right $ InputTypeAttributeTime
                                         _           -> Left $ T.concat ["TypeAttribute value not parsing -->",v]

  fromAttribute (Attribute other _) = wrongAttrResponse "type" other


defaultInput :: Input
defaultInput = Input defaultInputType defaultInputAttributesList

defaultInputType :: InputType
defaultInputType = InputTypeText $ InputText ("" :: T.Text)

defaultInputAttributesList :: [InputAttribute]
defaultInputAttributesList = [wAttr, tAttr, ixAttr,maxAttr,minAttr,rAttr]
              where wAttr = InputWidth $ WidthAttribute (12::Int)
                    ixAttr = InputIndexable $ IndexableAttribute True
                    minAttr = InputMinDouble $ MinAttributeDouble (0.0   :: Double)
                    maxAttr = InputMaxDouble $ MaxAttributeDouble (150.0 :: Double)
                    rAttr   = InputRequired $ RequiredAttribute True
                    tAttr = InputType $ InputTypeAttributeText
