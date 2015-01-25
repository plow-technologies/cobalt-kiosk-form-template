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


module Kiosk.Backend.Form.Attribute.Width () where

import Kiosk.Backend.Form.Attribute
import           Data.Aeson             (FromJSON, ToJSON)
import Data.Monoid ((<>))
import Data.Text (pack
                 ,unpack)
import           Text.Read              (readMaybe)                 
import           GHC.Generics           (Generic)
import Data.Either.Validation (Validation(..))
-- Width Attribute
data WidthAttr = WidthAttr {
                    _getWidth::Int
} deriving (Generic, Show, Ord, Eq)

instance ToJSON WidthAttr where
instance FromJSON WidthAttr where

instance AttributeClass WidthAttr where
   toAttribute (WidthAttr a) = Attribute "width" (pack ("'" ++ show a ++"'"))
   fromAttribute (Attribute "width" w) = case readMaybe (unpack w) of
                                    (Just w') -> Success . WidthAttr $ w'
                                    Nothing -> Failure $ pack "WidthAttribute value not parsing -->" <> w
   fromAttribute (Attribute other _) = wrongAttrResponse "width" other
