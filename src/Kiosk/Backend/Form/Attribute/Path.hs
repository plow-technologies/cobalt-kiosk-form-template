{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}

module Kiosk.Backend.Form.Attribute.Path (PathAttribute(..)) where

import Kiosk.Backend.Form.Attribute
import qualified Data.Text as T
import Text.Read   (readMaybe)
import           GHC.Generics           (Generic)

data PathAttribute = PathAttribute { 
	_getPath :: T.Text 
} deriving (Generic,Show,Eq,Ord)

instance AttributeClass PathAttribute where
  toAttribute (PathAttribute a) = Attribute "path" (T.pack ("'" ++ show a ++ "'"))
  fromAttribute (Attribute "path" v) = case readMaybe (T.unpack v) of
  									(Just v') -> Right (PathAttribute v')
  									Nothing -> Left $ T.concat ["PathAttribute value not parsing -->",v]
  fromAttribute (Attribute other _) = wrongAttrResponse "path" other