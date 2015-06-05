{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Kiosk.Backend.Form.Element.Phone ( Phone(..)
                                        , PhoneAttributes (..)
                                        , defaultPhone) where

import qualified Data.Text                          as T
import           GHC.Generics
import           Kiosk.Backend.Form.Attribute
import           Kiosk.Backend.Form.Attribute.Width
import           Text.Read                          (readMaybe)

data Phone = Phone {
  _getPhoneText :: T.Text,
  _phoneAttrib  :: [PhoneAttributes]
} deriving (Show, Ord, Eq,Generic)

-- Phone Attributes
data PhoneAttributes = PhoneWidth WidthAttribute deriving (Show, Ord, Eq,Generic)

instance AttributeClass PhoneAttributes where
  toAttribute (PhoneWidth a) = toAttribute a
  fromAttribute (Attribute "width" w) = case readMaybe (T.unpack w) of
    (Just w') -> Right (PhoneWidth $ WidthAttribute w')
    Nothing -> Left $ T.concat ["WidthAttribute value not parsing -->",w]
  fromAttribute _ = Left "Not a valid phone attribute"


defaultPhone :: Phone
defaultPhone = Phone "580-229-0067" [PhoneWidth $ WidthAttribute (12::Int) ]
