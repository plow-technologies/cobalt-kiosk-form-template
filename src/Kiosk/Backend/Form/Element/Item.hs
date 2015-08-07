{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}


module Kiosk.Backend.Form.Element.Item ( Item(..)
                                       , ItemType (..)
                                       , ItemAttributes (..)
                                       , defaultItem
                                       , defaultRadioItem) where

import           Kiosk.Backend.Form.Attribute
import           Kiosk.Backend.Form.Attribute.Required
import           Kiosk.Backend.Form.Attribute.Width

import           Data.Aeson                                      (FromJSON,
                                                                  ToJSON)
import qualified Data.Text                                       as T
import           GHC.Generics                                    (Generic)
import           Kiosk.Backend.Form.Element.Item.AutoInput
import           Kiosk.Backend.Form.Element.Item.Button
import           Kiosk.Backend.Form.Element.Item.Checkbox
import           Kiosk.Backend.Form.Element.Item.Dropdown
import           Kiosk.Backend.Form.Element.Item.EmptyBlock
import           Kiosk.Backend.Form.Element.Item.Input
import           Kiosk.Backend.Form.Element.Item.Label
import           Kiosk.Backend.Form.Element.Item.Checkbox
import           Kiosk.Backend.Form.Element.Item.Radio
import           Kiosk.Backend.Form.Element.Item.TableLeftHeader
import           Kiosk.Backend.Form.Element.Item.TableTopHeader
import           Text.Read                                       (readMaybe)

-- A Item containing different item type and its attirbutes
data Item = Item {
  _item       :: [ItemType],
  _itemAttrib :: [ItemAttributes]
} deriving (Generic, Show)


instance ToJSON Item where
instance FromJSON Item where
-- Item Attributes
data ItemAttributes = ItemWidth WidthAttribute | ItemRequired RequiredAttribute deriving (Generic, Show)

instance ToJSON ItemAttributes where
instance FromJSON ItemAttributes where
instance AttributeClass ItemAttributes where
    toAttribute (ItemWidth a) = toAttribute a
    toAttribute (ItemRequired a) = toAttribute a
    fromAttribute (Attribute "width" w) = case readMaybe (T.unpack w) of
                                              (Just w') -> Right (ItemWidth $ WidthAttribute w')
                                              Nothing -> Left $ T.concat ["WidthAttribute value of ItemWidth not parsing -->",w]
    fromAttribute (Attribute "required" w) = case readMaybe (T.unpack w) of
                                              (Just w') -> Right (ItemRequired $ RequiredAttribute w')
                                              Nothing -> Left $ T.concat ["ItemRequired value of RequiredAttribute not parsing -->",w]

    fromAttribute _ = Left "Not a valid item attribute"

data ItemType = ItemLabel Label
              | ItemInput Input
              | ItemAutoInput AutoInput
              | ItemButton Button
              | ItemRadio Radio
              | ItemCheckbox Checkbox
              | ItemDropdown Dropdown
              | ItemEmptyBlock EmptyBlock
              | ItemTableTopHeader TableTopHeader
              | ItemTableLeftHeader TableLeftHeader
    deriving (Generic, Show)

instance ToJSON ItemType where
instance FromJSON ItemType where

defaultItem :: Item
defaultItem = defaultInputItem

defaultInputItem :: Item
defaultInputItem = Item [ItemLabel defaultLabel, ItemInput defaultInput] [ItemWidth $ WidthAttribute (12::Int)]

defaultRadioItem :: Item
defaultRadioItem = Item [ItemRadio defaultRadio] []
