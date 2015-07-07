{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Kiosk.Backend.Form.Attribute.Required (RequiredAttribute(..)) where

import           Data.Aeson                   (FromJSON, ToJSON)
import qualified Data.Text                    as T
import           GHC.Generics                 (Generic)
import           Kiosk.Backend.Form.Attribute
import           Text.Read                    (readMaybe)

data RequiredAttribute = RequiredAttribute {
	_getRequired :: Bool
} deriving (Generic,Show,Eq,Ord)


instance ToJSON RequiredAttribute where
instance FromJSON RequiredAttribute where

instance AttributeClass RequiredAttribute where
  toAttribute (RequiredAttribute a) = Attribute "required" (T.pack ("'" ++ show a ++ "'"))
  fromAttribute (Attribute "required" v) = case readMaybe (T.unpack v) of
  									(Just v') -> Right (RequiredAttribute v')
  									Nothing -> Left $ T.concat ["RequiredAttribute value not parsing -->",v]
  fromAttribute (Attribute other _) = wrongAttrResponse "required" other