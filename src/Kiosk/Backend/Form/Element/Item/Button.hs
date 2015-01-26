{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}


{- |
Module      :  Kiosk.Backend.Form.Element.Item.Button
Description :  Button Element
Copyright   :  Plow Technologies LLC
License     :  MIT License

Maintainer  :  Scott Murphy
Stability   :  experimental
Portability :  portable

-}


module Kiosk.Backend.Form.Element.Item.Button (Button (..), defaultButton, defaultButtonAttributeList) where


import           Control.Applicative                 ((<$>), (<|>))
import           Data.Aeson                          (FromJSON, ToJSON)
import           Data.Either.Validation              (Validation (..))
import           Data.Text                           (Text)
import           GHC.Generics                        (Generic)
import           Kiosk.Backend.Form.Attribute        (AttributeClass (..))
import           Kiosk.Backend.Form.Attribute.Action
import           Kiosk.Backend.Form.Attribute.Width
-- Button is Text with set of attributes
data Button = Button {
   _getButtonText :: Text,
   _buttonAttrib  :: [ButtonAttributes]
} deriving (Generic, Show)

instance ToJSON Button where
instance FromJSON Button where


-- Button Atrributes
data ButtonAttributes = ButtonWidth WidthAttribute | ButtonAction ActionAttribute deriving (Generic, Show)

instance ToJSON ButtonAttributes where
instance FromJSON ButtonAttributes where

instance AttributeClass ButtonAttributes where
   toAttribute (ButtonWidth w) = toAttribute w
   toAttribute (ButtonAction a) = toAttribute a
   fromAttribute  = tryAllButtonAttributes
     where
       tryAllButtonAttributes a' = ButtonWidth <$> fromAttribute a'  <|>
                                   ButtonAction <$> fromAttribute a' <|>
                                   Failure "Not a valid button Attirbute"



defaultButton :: Button
defaultButton = Button "Submit Button" defaultButtonAttributeList

defaultButtonAttributeList :: [ButtonAttributes]
defaultButtonAttributeList = [ButtonWidth $ WidthAttribute (12::Int), ButtonAction $ ActionAttribute "sendjson"]
