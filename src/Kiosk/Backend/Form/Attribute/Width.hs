{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}


{- |
Module      :  Kiosk.Backend.Form.Attribute.Width
Description :  Width Attribute, determines width of an element
Copyright   :  Plow Technologies LLC 
License     :  MIT License

Maintainer  :  Scott Murphy
Stability   :  experimental
Portability :  portable

-}


module Kiosk.Backend.Form.Attribute.Width (WidthAttribute(..)) where

import Kiosk.Backend.Form.Attribute
import           Data.Aeson             (FromJSON, ToJSON)
import Data.Monoid ((<>))
import Data.Text (pack
                 ,unpack)
import           Text.Read              (readMaybe)                 
import           GHC.Generics           (Generic)
import Data.Either.Validation (Validation(..))
-- Width Attribute
data WidthAttribute = WidthAttribute {
                         _getWidth::Int
} deriving (Generic, Show, Ord, Eq)

instance ToJSON WidthAttribute where
instance FromJSON WidthAttribute where

instance AttributeClass WidthAttribute where
   toAttribute (WidthAttribute a) = Attribute "width" (pack ("'" ++ show a ++"'"))
   fromAttribute (Attribute "width" w) = case readMaybe (unpack w) of
                                    (Just w') -> Success . WidthAttribute $ w'
                                    Nothing -> Failure $ pack "WidthAttribute value not parsing -->" <> w
   fromAttribute (Attribute other _) = wrongAttrResponse "width" other
