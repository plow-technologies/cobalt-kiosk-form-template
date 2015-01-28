{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}


{- |
Module      :  Kiosk.Backend.Form.Element.Item.Input
Description :  Input Element, this is where json is created
Copyright   :  Plow Technologies LLC
License     :  MIT License

Maintainer  :  Scott Murphy
Stability   :  experimental
Portability :  portable



-}


module Kiosk.Backend.Form.Element.Item.Input ( Input(..)
                                             , InputType(..)
                                             , InputDouble (..)
                                             , InputInt (..)
                                             , Signature (..)
                                             , InputText(..)
                                             , InputDate(..)
                                             , InputTime(..)
                                             , InputAttribute(..)
                                             , InputTypeAttribute(..)
                                             , defaultInput
                                             , defaultInputType
                                             , defaultInputAttributesList) where

import           Control.Applicative                    ((<$>), (<|>))
import           Data.Aeson                             (FromJSON, ToJSON)
import           Data.Either.Validation                 (Validation (..))
import           Data.Monoid                            ((<>))
import           Data.Text                              (Text, pack)
import           Data.Typeable                          (Typeable)
import           GHC.Generics                           (Generic)
import           Kiosk.Backend.Form.Attribute           (Attribute (..),
                                                         AttributeClass (..),
                                                         wrongAttrResponse)
import           Kiosk.Backend.Form.Attribute.Indexable
import           Kiosk.Backend.Form.Attribute.Max
import           Kiosk.Backend.Form.Attribute.Min
import           Kiosk.Backend.Form.Attribute.Width

-- Input Type
data Input = Input {
              _getInput    :: InputType,
              _inputAttrib :: [InputAttribute]
} deriving (Generic, Show, Ord, Eq, Typeable)

instance ToJSON Input where
instance FromJSON Input where


-- Input type can be Text input or Signature input
data InputType = InputTypeText InputText
                |InputTypeSignature Signature
                |InputTypeInt InputInt
                |InputTypeDate InputDate
                |InputTypeTime InputTime
                |InputTypeDouble InputDouble deriving (Generic, Show, Ord, Eq, Typeable)

instance ToJSON InputType where
instance FromJSON InputType where

-- Text Type Input
newtype InputText = InputText { _getInputText::Text  } deriving (Generic, Show, Ord, Eq, Typeable)

instance ToJSON InputText where
instance FromJSON InputText where

-- Date Type :Just Text
newtype InputDate = InputDate { _getInputDate ::Text} deriving (Generic, Show, Ord, Eq, Typeable)
instance ToJSON InputDate where
instance FromJSON InputDate where


-- Time Type :Just Text
newtype InputTime = InputTime { _getInputTime ::Text} deriving (Generic, Show, Ord, Eq, Typeable)

instance ToJSON InputTime where
instance FromJSON InputTime where

-- Signature Type Input store as base64 encode Bytestring
newtype Signature = Signature {
_signature :: Text
 } deriving (Generic, Show, Ord, Eq, Typeable)

instance ToJSON Signature where
instance FromJSON Signature where

-- Number Type Input
newtype InputInt = InputInt { _getInputInt::Int  } deriving (Generic, Show, Ord, Eq, Typeable)

instance ToJSON InputInt where
instance FromJSON InputInt where

-- Number Type Double
newtype InputDouble = InputDouble { _getInputDouble::Double } deriving (Generic, Show, Ord, Eq, Typeable)

instance ToJSON InputDouble where
instance FromJSON InputDouble where

-- Input Attributes
data InputAttribute = InputWidth WidthAttribute
                    | InputType InputTypeAttribute
                    | InputIndexable IndexableAttribute
                    | InputMaxDouble MaxAttributeDouble
                    | InputMinDouble MinAttributeDouble
                   deriving (Generic, Show, Ord, Eq)

instance ToJSON InputAttribute where
instance FromJSON InputAttribute where

instance AttributeClass InputAttribute where
   toAttribute (InputWidth a) = toAttribute a
   toAttribute (InputIndexable a) = toAttribute a
   toAttribute (InputMaxDouble d) = toAttribute d
   toAttribute (InputMinDouble d) = toAttribute d
   toAttribute (InputType i) = toAttribute i
   fromAttribute = tryAllTypeAttributes
     where
       tryAllTypeAttributes a' = InputWidth <$>
                                 fromAttribute a' <|>
                                 InputType <$>
                                 fromAttribute a' <|>
                                 InputMaxDouble <$>
                                 fromAttribute a' <|>
                                 InputMinDouble <$>
                                 fromAttribute a' <|>
                                 InputIndexable <$>
                                 fromAttribute a' <|>
                                 Failure "Not a valid input Attribute"


-- InputConstraintAttribute


-- Type Attribute
data InputTypeAttribute = InputTypeAttributeDouble
                        | InputTypeAttributeInt
                        | InputTypeAttributeText
                        | InputTypeAttributeSignature
                        | InputTypeAttributeDate
                        | InputTypeAttributeTime
   deriving (Generic, Show, Ord, Eq)

instance ToJSON InputTypeAttribute where
instance FromJSON InputTypeAttribute where

-- instance AttributeClass InputType where

instance AttributeClass InputTypeAttribute where
   toAttribute InputTypeAttributeText  = Attribute "type" "'text'"
   toAttribute InputTypeAttributeSignature  = Attribute "type" "'signature'"
   toAttribute InputTypeAttributeInt  = Attribute "type" "'int'"
   toAttribute InputTypeAttributeDouble  = Attribute "type" "'double'"
   toAttribute InputTypeAttributeDate  = Attribute "type" "'date'"
   toAttribute InputTypeAttributeTime = Attribute "type" "'time'"
   fromAttribute (Attribute "type" v) = case v of
                                         "text" -> Success $ InputTypeAttributeText
                                         "signature" -> Success $ InputTypeAttributeSignature
                                         "int" -> Success $ InputTypeAttributeInt
                                         "double" -> Success$ InputTypeAttributeDouble
                                         "date" -> Success$ InputTypeAttributeDate
                                         "time" -> Success$ InputTypeAttributeTime
                                         _ -> Failure $ pack "TypeAttribute value not parsing -->" <> v
   fromAttribute (Attribute other _) = wrongAttrResponse "type" other


defaultInput :: Input
defaultInput = Input defaultInputType defaultInputAttributesList

defaultInputType :: InputType
defaultInputType = InputTypeText $ InputText (""::Text)

defaultInputAttributesList :: [InputAttribute]
defaultInputAttributesList = [wAttr, tAttr, ixAttr,maxAttr,minAttr]
              where wAttr = InputWidth $ WidthAttribute (12::Int)
                    ixAttr = InputIndexable $ IndexableAttribute True
                    minAttr = InputMinDouble $ MinAttributeDouble (0.0::Double)
                    maxAttr = InputMaxDouble $ MaxAttributeDouble (150.0::Double)
                    tAttr = InputType $ InputTypeAttributeText 
