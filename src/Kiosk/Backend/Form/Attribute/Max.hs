{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Kiosk.Backend.Form.Attribute.Max (MaxAttributeDouble(..)) where

import           Kiosk.Backend.Form.Attribute

import qualified Data.Text                    as T
import           GHC.Generics                 (Generic)
import           Text.Read                    (readMaybe)


data MaxAttributeDouble = MaxAttributeDouble {
	_getGenericMaxAmt :: Double
} deriving (Show,Eq,Ord,Generic)

instance AttributeClass MaxAttributeDouble where
  toAttribute (MaxAttributeDouble d) = Attribute "maxd" (T.pack ("'" ++ show d ++ "'"))
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
