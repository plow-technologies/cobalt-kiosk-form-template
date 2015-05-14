{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}


module Kiosk.Backend.Form.Element.Item ( Item(..)
                                       , ItemType (..)
                                       , ItemAttributes (..)
                                       , defaultItem
                                       , defaultRadioItem) where

import           Kiosk.Backend.Form.Attribute
import           Kiosk.Backend.Form.Attribute.Width

import           Kiosk.Backend.Form.Element.Item.Button
import           Kiosk.Backend.Form.Element.Item.EmptyBlock
import           Kiosk.Backend.Form.Element.Item.Input
import           Kiosk.Backend.Form.Element.Item.Label
import           Kiosk.Backend.Form.Element.Item.Radio
import           Kiosk.Backend.Form.Element.Item.TableLeftHeader
import           Kiosk.Backend.Form.Element.Item.TableTopHeader

import GHC.Generics (Generic)
import qualified Data.Text as T
import Text.Read   (readMaybe)

-- A Item containing different item type and its attirbutes
data Item = Item {
  _item       :: [ItemType],
  _itemAttrib :: [ItemAttributes]
} deriving (Generic, Show)

-- Item Attributes
data ItemAttributes = ItemWidth WidthAttribute deriving (Generic, Show)

instance AttributeClass ItemAttributes where
   toAttribute (ItemWidth a) = toAttribute a
   fromAttribute (Attribute "width" w) = case readMaybe (T.unpack w) of
                                              (Just w') -> Right (ItemWidth $ WidthAttribute w')
                                              Nothing -> Left $ T.concat ["WidthAttribute value of ItemWidth not parsing -->",w]
   fromAttribute _ = Left "Not a valid item attribute"

data ItemType = ItemLabel Label
              | ItemInput Input
              | ItemAutoInput Input
              | ItemButton Button
              | ItemRadio Radio 
              | ItemEmptyBlock EmptyBlock
              | ItemTableTopHeader TableTopHeader
              | ItemTableLeftHeader TableLeftHeader  
    deriving (Generic, Show)                                            

defaultItem :: Item
defaultItem = defaultInputItem

defaultInputItem :: Item
defaultInputItem = Item [ItemLabel defaultLabel, ItemInput defaultInput] [ItemWidth $ WidthAttribute (12::Int)]

defaultRadioItem :: Item
defaultRadioItem = Item [ItemRadio defaultRadio] []