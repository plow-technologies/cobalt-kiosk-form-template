{-# LANGUAGE OverloadedStrings #-}

module Kiosk.Backend.Form.Attribute.Max (MaxAttributeDouble(..)) where

import Kiosk.Backend.Form.Attribute
import qualified Data.Text as T
import Text.Read   (readMaybe)

data MaxAttributeDouble = MaxAttributeDouble { 
	_getGenericMaxAmt :: Double 
} deriving (Show,Eq,Ord)

instance AttributeClass MaxAttributeDouble where
  toAttribute (MaxAttributeDouble d) = Attribute "width" (T.pack ("'" ++ show d ++ "'"))
  fromAttribute (Attribute "maxd" m) = case readMaybe (T.unpack m) of
  									(Just m') -> Right (MaxAttributeDouble m')
  									Nothing -> Left $ T.concat ["MaxAttribute value not parsing -->",m]
  fromAttribute (Attribute other _) = wrongAttrResponse "maxd" other