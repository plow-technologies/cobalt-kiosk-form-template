{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}


module Kiosk.Backend.Form.Element.Item.Radio where


import           Kiosk.Backend.Form.Attribute
import           Kiosk.Backend.Form.Attribute.Required
import qualified Data.Text                                       as T
import           Data.Typeable                          (Typeable)

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
  , _getRadioAttribs   :: [RadioAttribute]
  , _getRadioOptions   :: [Option]
  , _getRadioQualifier :: [OptionQualifier]
} deriving (Generic, Show, Typeable)

-- Item Attributes
data RadioAttribute = RadioRequired RequiredAttribute deriving (Generic, Show, Ord, Eq, Typeable)

instance ToJSON Radio where
instance FromJSON Radio where

instance ToJSON RadioAttribute where
instance FromJSON RadioAttribute where

instance AttributeClass RadioAttribute where
    toAttribute (RadioRequired a) = toAttribute a
    {-
    fromAttribute (Attribute "required" w) = case readMaybe (T.unpack w) of
                                              (Just w') -> Right (RadioRequired $ RequiredAttribute w')
                                              Nothing -> Left $ T.concat ["RadioRequired value of RequiredAttribute not parsing -->",w]
    -}
    fromAttribute (Attribute t v) = case t of
                                      "required" -> case (readMaybe (T.unpack v)) of
                                                     (Just v') -> Right  $ RadioRequired $ RequiredAttribute v'
                                                     Nothing   -> Left $ T.concat ["RadioRequired value not parsing -->",t,v]
                                      _ -> Left $ T.concat ["RadioRequired value not parsing -->",t,v]

defaultRadio :: Radio
defaultRadio = Radio defaultLabel [] [defaultOption] [defaultOptionQualifier]

