{- |
Module      :  Kiosk.Backend.Element.Address
Description :  Address Element Types for Kiosk
Copyright   :  (c) <Plow Technology 2014>
License     :  <MIT>
Maintainer  :  <lingpo.huang@plowtech.net>
Stability   :  unstable
Portability :  portable
<Haskell Types for the form sending from the SWIF Application>
-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}

module Kiosk.Backend.Form.Element.Address() where 

import           Data.Aeson                   (FromJSON, ToJSON)
import           Data.Typeable                (Typeable)
import           GHC.Generics                 (Generic)
import           Control.Applicative    ((<$>), (<|>))
import           Data.Either.Validation (Validation(..))
import           Data.Text                    (Text)  
import           Kiosk.Backend.Form.Attribute.Width
import           Kiosk.Backend.Form.Attribute
-- A Address contain address information of the Company
data Address = Address {
               _getAddressText :: Text,
               _addressAttrib  :: [AddressAttributes]
                       } deriving (Generic, Show, Ord, Eq, Typeable)

-- Address Attributes
data AddressAttributes = AddressWidth WidthAttribute
                           deriving (Generic, Show, Ord, Eq)


instance ToJSON AddressAttributes where
instance FromJSON AddressAttributes where

instance AttributeClass AddressAttributes where
   toAttribute (AddressWidth a) = toAttribute a
   fromAttribute  = tryAllAddressAttributes
     where
       tryAllAddressAttributes a' = AddressWidth <$> fromAttribute a' <|> Failure "Not a valid Address Attirbute"
