{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Kiosk.Backend.Form.Element.Address( Address(..)
                                         , AddressAttributes (..)
                                         , defaultAddress) where

import qualified Data.Text                          as T
import           GHC.Generics
import           Kiosk.Backend.Form.Attribute
import           Kiosk.Backend.Form.Attribute.Width
import           Text.Read                          (readMaybe)

-- A Address contain address information of the Company
data Address = Address {
  _getAddressText :: T.Text,
  _addressAttrib  :: [AddressAttributes]
} deriving (Show, Ord, Eq,Generic)

data AddressAttributes = AddressWidth WidthAttribute deriving (Show, Ord, Eq,Generic)


instance AttributeClass AddressAttributes where
  toAttribute (AddressWidth a) = toAttribute a
  fromAttribute (Attribute "width" w) = case readMaybe (T.unpack w) of
                                    (Just w') -> Right (AddressWidth (WidthAttribute w'))
                                    Nothing -> Left $ T.concat ["AddressAttributes value not parsing -->",w]
  fromAttribute (Attribute other _) = wrongAttrResponse "width" other

defaultAddress :: Address
defaultAddress = Address "PO Box 130 Wilson, Oklahoma 73463\n886-849-5483\nAnswering Service 580-220-9936" [AddressWidth $ WidthAttribute (12::Int)]


