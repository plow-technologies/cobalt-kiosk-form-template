
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}


module Kiosk.Backend.Form.Element.Item.Radio  ( Radio(..)
                                              , defaultRadio) where


import           GHC.Generics                           (Generic)


import           Kiosk.Backend.Form.Element.Item.Label  (Label (..),
                                                         defaultLabel)

import           Data.Aeson                             (FromJSON, ToJSON)


import           Kiosk.Backend.Form.Element.Item.Option (Option (..),
                                                         OptionQualifier (..),
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

