{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Kiosk.Backend.Form.Element.Constant( Constant(..)
                                          , ConstantAttributes(..)
                                          , defaultConstant) where

import           Data.Aeson                             (FromJSON, ToJSON)
import qualified Data.Text                              as T
import           GHC.Generics
import           Kiosk.Backend.Form.Attribute
import           Kiosk.Backend.Form.Attribute.Indexable
-- Button is Text with set of attributes
data Constant = Constant {
  _getConstantText :: T.Text,
  _constantAttrib  :: [ConstantAttributes]
} deriving (Show,Generic)


instance ToJSON Constant where
instance FromJSON Constant where
-- Button Atrributes
data ConstantAttributes = ConstantAttributeType T.Text | ConstantAttributeIndexable IndexableAttribute deriving (Show,Generic)

instance ToJSON ConstantAttributes where
instance FromJSON ConstantAttributes where
instance AttributeClass ConstantAttributes where
  toAttribute   (ConstantAttributeType t) = Attribute "type" t
  toAttribute   (ConstantAttributeIndexable i) = toAttribute i
  fromAttribute (Attribute "type" i) =  Right $ ConstantAttributeType $ i
  fromAttribute _ = Left "Not a valid button Attribute"


defaultConstant :: Constant
defaultConstant = Constant "Black Watch" [ ConstantAttributeType "'Company'", ConstantAttributeIndexable $ IndexableAttribute True ]
