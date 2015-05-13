{-# LANGUAGE OverloadedStrings #-}

module Kiosk.Backend.Form.Attribute.Action (ActionAttribute(..)) where

import Kiosk.Backend.Form.Attribute
import qualified Data.Text as T
import Text.Read   (readMaybe)

data ActionAttribute = ActionAttribute { 
	_getFunctionName :: T.Text 
} deriving (Show,Eq,Ord)

instance AttributeClass ActionAttribute where
  toAttribute (ActionAttribute a) = Attribute "action" (T.pack ("'" ++ show a ++ "'"))
  fromAttribute (Attribute "action" v) = case readMaybe (T.unpack v) of
  									(Just v') -> Right (ActionAttribute v')
  									Nothing -> Left $ T.concat ["ActionAttribute value not parsing -->",v]
  fromAttribute (Attribute other _) = wrongAttrResponse "action" other