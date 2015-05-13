{-# LANGUAGE OverloadedStrings #-}

module Kiosk.Backend.Form.Element.Item.Button ( Button (..)
	                                          , defaultButton
	                                          , defaultButtonAttributeList) where

import           Kiosk.Backend.Form.Attribute
import           Kiosk.Backend.Form.Attribute.Action
import           Kiosk.Backend.Form.Attribute.Width
import qualified Data.Text as T
import Text.Read   (readMaybe)

-- Button is Text with set of attributes
data Button = Button {
   _getButtonText :: T.Text,
   _buttonAttrib  :: [ButtonAttributes]
} deriving (Show)

data ButtonAttributes = ButtonWidth WidthAttribute | ButtonAction ActionAttribute deriving (Show)


instance AttributeClass ButtonAttributes where
   toAttribute (ButtonWidth w) = toAttribute w
   toAttribute (ButtonAction a) = toAttribute a
   fromAttribute (Attribute "width" w) = case readMaybe (T.unpack w) of
                                              (Just w') -> Right (ButtonWidth $ WidthAttribute w')
                                              Nothing -> Left $ T.concat ["WidthAttribute value not parsing -->",w]
   fromAttribute (Attribute "action" v) = case readMaybe (T.unpack v) of
  									           (Just v') -> Right (ButtonAction $ ActionAttribute v')
  									           Nothing -> Left $ T.concat ["ActionAttribute value not parsing -->",v]

   fromAttribute _ = Left "Not a valid button attribute"

defaultButton :: Button
defaultButton = Button "Submit Button" defaultButtonAttributeList

defaultButtonAttributeList :: [ButtonAttributes]
defaultButtonAttributeList = [ButtonWidth $ WidthAttribute (12::Int), ButtonAction $ ActionAttribute "sendjson"]