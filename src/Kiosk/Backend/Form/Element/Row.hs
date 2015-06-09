{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Kiosk.Backend.Form.Element.Row ( Row(..)
                                      , RowAttributes(..)
                                      , defaultRows) where
import           Data.Aeson                         (FromJSON, ToJSON)
import qualified Data.Text                          as T
import           GHC.Generics
import           Kiosk.Backend.Form.Attribute
import           Kiosk.Backend.Form.Attribute.Width
import           Kiosk.Backend.Form.Element.Item
import           Text.Read                          (readMaybe)
-- A row containing a list of items and its attributes
data Row = Row {
 _rowItem   :: [Item],
 _rowAttrib :: [RowAttributes]
} deriving (Show,Generic)


instance ToJSON Row where
instance FromJSON Row where
-- Row Attributes
data RowAttributes = RowWidth WidthAttribute deriving (Show,Generic)

instance AttributeClass RowAttributes where
   toAttribute (RowWidth a) = toAttribute a
   fromAttribute (Attribute "width" w) = case readMaybe (T.unpack w) of
                                              (Just w') -> Right (RowWidth $ WidthAttribute w')
                                              Nothing -> Left $ T.concat ["WidthAttribute value of Row not parsing -->",w]
   fromAttribute _ = Left "Not a valid row attribute"



instance ToJSON RowAttributes where
instance FromJSON RowAttributes where

defaultRows :: [Row]
defaultRows =  [Row [defaultItem] [RowWidth $ WidthAttribute (12::Int)]]
