
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}


module Kiosk.Backend.Form.Element.Item.Radio  ( Radio(..)
                                              , defaultRadio) where

import qualified Data.Text                              as T
import           GHC.Generics                           (Generic)
import           Kiosk.Backend.Form.Attribute           (Attribute (..),
                                                         AttributeClass (..))
import           Kiosk.Backend.Form.Element.Item.Input
import           Kiosk.Backend.Form.Element.Item.Label  (Label (..),
                                                         defaultLabel)

import           Data.Aeson                             (FromJSON, ToJSON)
import           Kiosk.Backend.Form.Attribute.Indexable
import           Kiosk.Backend.Form.Attribute.Width
import           Kiosk.Backend.Form.Element.Item.Option (Option (..),
                                                         OptionAttributes (..),
                                                         OptionQualifier (..), OptionQualifierAttributes (..),
                                                         QualifierChoices (..),
                                                         defaultOption,
                                                         defaultOptionQualifier)
-- A Radio button is a circular, singular selector
-- Our radio buttons come preloaded with Options!


-- |Radio Button parent element
data Radio = Radio {
    _getRadioLabel     :: Label
  , _getRadioOptions   :: [Option]
  , _getRadioQualifier :: [OptionQualifier]
} deriving (Generic, Show)


instance ToJSON Radio where
instance FromJSON Radio where
defaultRadio :: Radio
defaultRadio = Radio defaultLabel [defaultOption] [defaultOptionQualifier]

