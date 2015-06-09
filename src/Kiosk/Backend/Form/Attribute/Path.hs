{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Kiosk.Backend.Form.Attribute.Path (PathAttribute(..)) where

import Kiosk.Backend.Form.Attribute
import qualified Data.Text as T
import Text.Read   (readMaybe)
import           GHC.Generics           (Generic)
import Data.Monoid
import           Data.Aeson                   (FromJSON, ToJSON)

import qualified Data.Text                    as T
import           GHC.Generics                 (Generic)
import           Kiosk.Backend.Form.Attribute
import           Text.Read                    (readMaybe)

data PathAttribute = PathAttribute {
	_getPath :: T.Text
} deriving (Generic,Show,Eq,Ord)

instance AttributeClass PathAttribute where
  toAttribute (PathAttribute a) = Attribute "path" ("'" <> a <> "'")
  fromAttribute (Attribute "path" v) = case readMaybe (T.unpack v) of
  									(Just v') -> Right (PathAttribute v')
  									Nothing -> Left $ T.concat ["PathAttribute value not parsing -->",v]
  fromAttribute (Attribute other _) = wrongAttrResponse "path" other



instance ToJSON PathAttribute where
instance FromJSON PathAttribute where



