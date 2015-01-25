{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}


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
                                             , InputAttribute(..)
                                             , InputTypeAttribute(..)
                                             , defaultInput) where


import Data.Aeson (ToJSON
                  ,FromJSON)
import GHC.Generics (Generic)
import Data.Text (Text
                 ,pack)
import Data.Monoid ((<>))
import Kiosk.Backend.Form.Attribute (AttributeClass(..)
                                    ,Attribute(..)
                                    ,wrongAttrResponse)
import Data.Typeable (Typeable)
import Kiosk.Backend.Form.Attribute.Width 

import           Data.Either.Validation (Validation(..))
import           Control.Applicative    ((<$>), (<|>))
-- Input Type
data Input = Input {
              _getInput    :: InputType,
              _inputAttrib :: [InputAttribute]

} deriving (Generic, Show, Ord, Eq, Typeable)

instance ToJSON Input where
instance FromJSON Input where

-- Input type canbe Text input or Signature input
data InputType = InputTypeText InputText
                |InputTypeSignature Signature
                |InputTypeInt InputInt
                |InputTypeDouble InputDouble deriving (Generic, Show, Ord, Eq, Typeable)

instance ToJSON InputType where
instance FromJSON InputType where

-- Text Type Input
newtype InputText = InputText { _getInputText::Text  } deriving (Generic, Show, Ord, Eq, Typeable)

instance ToJSON InputText where
instance FromJSON InputText where

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
data InputAttribute = InputWidth WidthAttribute | InputType InputTypeAttribute deriving (Generic, Show, Ord, Eq)

instance ToJSON InputAttribute where
instance FromJSON InputAttribute where

instance AttributeClass InputAttribute where
   toAttribute (InputWidth a) = toAttribute a
   toAttribute (InputType i) = toAttribute i
   fromAttribute = tryAllTypeAttributes
     where
       tryAllTypeAttributes a' = InputWidth <$> fromAttribute a' <|> InputType <$> fromAttribute a' <|> Failure "Not a valid input Attribute"


-- Type Attribute
data InputTypeAttribute = InputTypeAttribute {
   _getTypeName :: InputType
} deriving (Generic, Show, Ord, Eq)

instance ToJSON InputTypeAttribute where
instance FromJSON InputTypeAttribute where

-- instance AttributeClass InputType where

instance AttributeClass InputTypeAttribute where
   toAttribute (InputTypeAttribute (InputTypeText _)) = Attribute "type" "'text'"
   toAttribute (InputTypeAttribute (InputTypeSignature _)) = Attribute "type" "'signature'"
   toAttribute (InputTypeAttribute (InputTypeInt _)) = Attribute "type" "int"
   toAttribute (InputTypeAttribute (InputTypeDouble _)) = Attribute "type" "double"                                               
   fromAttribute (Attribute "type" v) = case v of
                                         "text" -> Success . InputTypeAttribute . InputTypeText . InputText $  v
                                         "signature" -> Success . InputTypeAttribute . InputTypeSignature . Signature $ v
                                         "int" -> Success . InputTypeAttribute . InputTypeInt . InputInt $ 0
                                         "double" -> Success. InputTypeAttribute . InputTypeDouble . InputDouble $ 0.0
                                         _ -> Failure $ pack "TypeAttribute value not parsing -->" <> v
   fromAttribute (Attribute other _) = wrongAttrResponse "type" other


defaultInput :: Input
defaultInput = Input defaultInputType defaultInputAttributesList

defaultInputType :: InputType
defaultInputType = InputTypeText $ InputText (""::Text)

defaultInputAttributesList :: [InputAttribute]
defaultInputAttributesList = [wAttr, tAttr]
              where wAttr = InputWidth $ WidthAttribute (12::Int)
                    tAttr = InputType $ InputTypeAttribute defaultInputType
