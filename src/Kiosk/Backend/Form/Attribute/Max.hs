{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Kiosk.Backend.Form.Attribute.Max (MaxAttributeDouble(..)) where

import Kiosk.Backend.Form.Attribute

import           GHC.Generics           (Generic)
import qualified Data.Text as T
import           Text.Read              (readMaybe)
import           Data.Text.Read         (double)

data MaxAttributeDouble = MaxAttributeDouble { 
	_getGenericMaxAmt :: Double 
} deriving (Show,Eq,Ord,Generic)

instance AttributeClass MaxAttributeDouble where
  toAttribute (MaxAttributeDouble d) = Attribute "width" (T.pack ("'" ++ show d ++ "'"))
  fromAttribute (Attribute "maxd" m) = case readMaybe (T.unpack m) of
  									        (Just m') -> Right (MaxAttributeDouble m')
  									        Nothing -> Left $ T.concat ["MaxAttribute value not parsing -->",m]
  									{-
  									 case readMaybe (T.unpack m) of
  									(Just m') -> Right (MaxAttributeDouble m')
  									Nothing -> Left $ T.concat ["MaxAttribute value not parsing -->",m]

  									case double m of
  											Right (m',_) -> Right (MaxAttributeDouble m')
  											Left  _        -> Left $ T.concat ["MaxAttribute value not parsing -->",m]
  									
  									-}
  fromAttribute (Attribute other _) = wrongAttrResponse "maxd" other