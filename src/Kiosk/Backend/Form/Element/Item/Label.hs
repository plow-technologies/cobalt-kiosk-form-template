{-# LANGUAGE OverloadedStrings #-}

module Kiosk.Backend.Form.Element.Item.Label( Label (..)
                                            , LabelAttributes(..)
                                            , defaultLabel) where
import Kiosk.Backend.Form.Attribute.Width
import Kiosk.Backend.Form.Attribute
import qualified Data.Text as T
import Text.Read (readMaybe)

-- A label is a text with set of attributes
data Label = Label {
  _getLabelText :: T.Text,
  _labelAttrib  :: [LabelAttributes]
} deriving (Show, Ord, Eq)

-- Label Attributes
data LabelAttributes = LabelWidth WidthAttribute deriving (Show, Ord, Eq)

instance AttributeClass LabelAttributes where
    toAttribute (LabelWidth a) = toAttribute a
    fromAttribute (Attribute "width" w) = case readMaybe (T.unpack w) of
       (Just w') -> Right (LabelWidth $ WidthAttribute w')
       Nothing -> Left $ T.concat ["WidthAttribute value of LabelAttributes not parsing -->",w]
    fromAttribute _ = Left  "Not a valid LabelAttributes Attribute"


defaultLabel :: Label
defaultLabel = Label "Legal Dest" [LabelWidth $ WidthAttribute (12::Int)]

{-
data Item = Item {
  _item       :: [ItemType],
  _itemAttrib :: [ItemAttributes]
} deriving (Show)

-- Item Attributes
data ItemAttributes = ItemWidth WidthAttribute deriving (Show)

instance AttributeClass ItemAttributes where
   toAttribute (ItemWidth a) = toAttribute a
   fromAttribute (Attribute "width" w) = case readMaybe (T.unpack w) of
                                              (Just w') -> Right (ItemWidth $ WidthAttribute w')
                                              Nothing -> Left $ T.concat ["WidthAttribute value of ItemWidth not parsing -->",w]
   fromAttribute _ = Left "Not a valid item attribute"
-}