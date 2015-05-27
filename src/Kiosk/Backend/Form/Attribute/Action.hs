{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Kiosk.Backend.Form.Attribute.Action (ActionAttribute(..)) where

import qualified Data.Text                    as T
import           GHC.Generics                 (Generic)
import           Kiosk.Backend.Form.Attribute


data ActionAttribute = ActionAttribute {
	_getFunctionName :: T.Text
} deriving (Generic,Show,Ord,Eq)

instance AttributeClass ActionAttribute where
  toAttribute (ActionAttribute a) = Attribute "action" (T.pack ("'" ++ show a ++ "'"))
  fromAttribute (Attribute "action" v) = Right $ ActionAttribute v
  	{-
  	case readMaybe (T.unpack v) of
  									(Just v') -> Right (ActionAttribute v')
  									Nothing -> Left $ T.concat ["ActionAttribute value not parsing -->",v]
    -}
  fromAttribute (Attribute other _) = wrongAttrResponse "action" other
