{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Kiosk.Backend.Form.Attribute.Indexable (IndexableAttribute(..)) where

import           Data.Aeson                   (FromJSON, ToJSON)
import qualified Data.Text                    as T
import           GHC.Generics                 (Generic)
import           Kiosk.Backend.Form.Attribute
import           Text.Read                    (readMaybe)
data IndexableAttribute = IndexableAttribute {
	_getIndexable :: Bool
} deriving (Generic,Show,Eq,Ord)



instance ToJSON IndexableAttribute where
instance FromJSON IndexableAttribute where

instance AttributeClass IndexableAttribute where
  toAttribute (IndexableAttribute a) = Attribute "indexable" (T.pack ("'" ++ show a ++ "'"))
  fromAttribute (Attribute "indexable" v) = case readMaybe (T.unpack v) of
  									(Just v') -> Right (IndexableAttribute v')
  									Nothing -> Left $ T.concat ["IndexableAttribute value not parsing -->",v]
  fromAttribute (Attribute other _) = wrongAttrResponse "indexable" other
