{- |
Module      :  Kiosk.Backend.Element.Phone
Description :  Form Element Types for Kiosk
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

module Kiosk.Backend.Form.Element.Phone ( Phone(..)
                                          , PhoneAttributes (..)
                                          , defaultPhone) where 

import           Data.Aeson                   (FromJSON, ToJSON)
import           Data.Typeable                (Typeable)
import           GHC.Generics                 (Generic)
import           Control.Applicative    ((<$>), (<|>))
import           Data.Either.Validation (Validation(..))
import           Data.Text                    (Text)  
import           Kiosk.Backend.Form.Attribute.Width
import           Kiosk.Backend.Form.Attribute
-- A Phone mainly is the Name of the Phone
data Phone = Phone {
               _getPhoneText :: Text,
               _phoneAttrib  :: [PhoneAttributes]
                       } deriving (Generic, Show, Ord, Eq, Typeable)

instance ToJSON Phone where
instance FromJSON Phone where


-- Phone Attributes
data PhoneAttributes = PhoneWidth WidthAttribute
                           deriving (Generic, Show, Ord, Eq)

instance ToJSON PhoneAttributes where
instance FromJSON PhoneAttributes where

instance AttributeClass PhoneAttributes where
   toAttribute (PhoneWidth a) = toAttribute a
   fromAttribute  = tryAllPhoneAttributes
     where
       tryAllPhoneAttributes a' = PhoneWidth <$> fromAttribute a' <|> Failure "Not a valid Phone Attirbute"


defaultPhone :: Phone
defaultPhone = Phone "580-229-0067" [PhoneWidth $ WidthAttribute (12::Int) ]
