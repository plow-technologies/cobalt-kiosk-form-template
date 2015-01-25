{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}


{- |
Module      :  Kiosk.Backend.Form.Element.Item.Label
Description :  Label Element
Copyright   :  Plow Technologies LLC 
License     :  MIT License

Maintainer  :  Scott Murphy
Stability   :  experimental
Portability :  portable

-}


module Kiosk.Backend.Form.Element.Item.Label(Label (..)
                                            ,defaultLabel) where


import Data.Aeson (ToJSON
                  ,FromJSON)
import GHC.Generics (Generic)
import Data.Text (Text)
import Kiosk.Backend.Form.Attribute (AttributeClass(..))
import           Data.Either.Validation (Validation(..))
import           Control.Applicative    ((<$>), (<|>))
import Kiosk.Backend.Form.Attribute.Width 

-- A label is a text with set of attributes
data Label = Label {
                    _getLabelText :: Text,
                    _labelAttrib  :: [LabelAttributes]
                    } deriving (Generic, Show)

instance ToJSON Label where
instance FromJSON Label where

-- | Attributes warpper type

-- Label Attributes
data LabelAttributes = LabelWidth WidthAttribute
                            deriving (Generic, Show)

instance ToJSON LabelAttributes where
instance FromJSON LabelAttributes where

instance AttributeClass LabelAttributes where
   toAttribute (LabelWidth a) = toAttribute a
   fromAttribute  = tryAllLabelAttributes
     where
       tryAllLabelAttributes a' = LabelWidth <$> fromAttribute a' <|> Failure "Not a valid Row Attirbute"


defaultLabel :: Label
defaultLabel = Label "Legal Dest" [LabelWidth $ WidthAttribute (12::Int)]

