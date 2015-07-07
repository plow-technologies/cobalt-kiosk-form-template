
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}


module Kiosk.Backend.Form.Element.Item.Radio where


import           Kiosk.Backend.Form.Attribute
import           Kiosk.Backend.Form.Attribute.Required
import qualified Data.Text                                       as T
import           Text.Read                                       (readMaybe)

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
  , _getRadioAttribs   :: [RadioAttributes]
  , _getRadioOptions   :: [Option]
  , _getRadioQualifier :: [OptionQualifier]
} deriving (Generic, Show)

-- Item Attributes
data RadioAttributes = RadioRequired RequiredAttribute deriving (Generic, Show)

instance ToJSON Radio where
instance FromJSON Radio where

instance ToJSON RadioAttributes where
instance FromJSON RadioAttributes where

instance AttributeClass RadioAttributes where
    toAttribute (RadioRequired a) = toAttribute a
    fromAttribute (Attribute "required" w) = case readMaybe (T.unpack w) of
                                              (Just w') -> Right (RadioRequired $ RequiredAttribute w')
                                              Nothing -> Left $ T.concat ["RadioRequired value of RequiredAttribute not parsing -->",w]

    fromAttribute _ = Left "Not a valid item attribute"

defaultRadio :: Radio
defaultRadio = Radio defaultLabel [] [defaultOption] [defaultOptionQualifier]

