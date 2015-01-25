{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}


{- |
Module      :  Kiosk.Backend.Form.Attribute.Action
Description :  Action Attribute, determines action of an element
Copyright   :  Plow Technologies LLC 
License     :  MIT License

Maintainer  :  Scott Murphy
Stability   :  experimental
Portability :  portable

-}


module Kiosk.Backend.Form.Attribute.Action (ActionAttribute(..)) where

import Kiosk.Backend.Form.Attribute
import           Data.Aeson             (FromJSON, ToJSON)
import Data.Monoid ((<>))
import Data.Text (pack
                 ,unpack)
import           Text.Read              (readMaybe)                 
import           GHC.Generics           (Generic)
import Data.Either.Validation (Validation(..))

-- Action Attribute
data ActionAttribute = ActionAttribute {
   _getFunctionName :: String
} deriving (Generic, Show)

instance ToJSON ActionAttribute where
instance FromJSON ActionAttribute where

instance AttributeClass ActionAttribute where
   toAttribute (ActionAttribute a) = Attribute "action" (pack.show $ a)
   fromAttribute (Attribute "action" v) = Success. ActionAttribute . unpack $ v
   fromAttribute (Attribute other _) = wrongAttrResponse "action" other


