{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Kiosk.Backend.Form.Element.Item.Label( Label (..)
                                            , LabelAttributes(..)
                                            , defaultLabel) where
import           Data.Aeson                         (FromJSON, ToJSON)
import qualified Data.Text                          as T
import           GHC.Generics                       (Generic)
import           Kiosk.Backend.Form.Attribute
import           Kiosk.Backend.Form.Attribute.Width
import           Text.Read                          (readMaybe)
-- A label is a text with set of attributes
data Label = Label {
  _getLabelText :: T.Text,
  _labelAttrib  :: [LabelAttributes]
} deriving (Generic, Show)

instance ToJSON Label where
instance FromJSON Label where
-- Label Attributes
data LabelAttributes = LabelWidth WidthAttribute deriving (Show, Ord, Eq, Generic)

instance ToJSON LabelAttributes where
instance FromJSON LabelAttributes where
instance AttributeClass LabelAttributes where
    toAttribute (LabelWidth a) = toAttribute a
    fromAttribute (Attribute "width" w) = case readMaybe (T.unpack w) of
       (Just w') -> Right (LabelWidth $ WidthAttribute w')
       Nothing -> Left $ T.concat ["WidthAttribute value of LabelAttributes not parsing -->",w]
    fromAttribute _ = Left  "Not a valid LabelAttributes Attribute"


defaultLabel :: Label
defaultLabel = Label "Legal Dest" [LabelWidth $ WidthAttribute (12::Int)]
