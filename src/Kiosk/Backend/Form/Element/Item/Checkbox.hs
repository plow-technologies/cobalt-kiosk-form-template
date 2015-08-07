{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}


module Kiosk.Backend.Form.Element.Item.Checkbox where


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
                                                         defaultOption)

import           Kiosk.Backend.Form.Element.Item.Dropdown (Dropdown,
                                                           defaultDropdown)
-- A Checkbox button is a square, singular or multiple selector


-- |Checkbox Button parent element
data Checkbox = Checkbox {
    _getCheckboxLabel     :: Label
  , _getCheckboxAttribs   :: [CheckboxAttribute]
  , _getCheckboxOptions   :: [Option]
  , _getCheckboxDropdown  :: Maybe Dropdown
} deriving (Generic, Show, Typeable)

-- Item Attributes
data CheckboxAttribute = CheckboxRequired RequiredAttribute deriving (Generic, Show, Ord, Eq, Typeable)

instance ToJSON Checkbox where
instance FromJSON Checkbox where

instance ToJSON CheckboxAttribute where
instance FromJSON CheckboxAttribute where

instance AttributeClass CheckboxAttribute where
    toAttribute (CheckboxRequired a) = toAttribute a

    fromAttribute (Attribute t v) = case t of
                                      "required" -> case (readMaybe (T.unpack v)) of
                                                     (Just v') -> Right  $ CheckboxRequired $ RequiredAttribute v'
                                                     Nothing   -> Left $ T.concat ["CheckboxRequired value not parsing -->",t,v]
                                      _ -> Left $ T.concat ["CheckboxRequired value not parsing -->",t,v]

defaultCheckbox :: Checkbox
defaultCheckbox = Checkbox defaultLabel [] [defaultOption] (Just defaultDropdown)
