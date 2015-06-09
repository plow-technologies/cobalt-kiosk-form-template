{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Kiosk.Backend.Form.Attribute.Width (WidthAttribute(..)) where
import           Data.Aeson                   (FromJSON, ToJSON)
import qualified Data.Text                    as T
import           GHC.Generics
import           Kiosk.Backend.Form.Attribute
import           Text.Read                    (readMaybe)

data WidthAttribute = WidthAttribute {
    _getWidth :: Int
} deriving (Show, Ord, Eq,Generic)

instance ToJSON WidthAttribute where
instance FromJSON WidthAttribute where
instance AttributeClass WidthAttribute where
   toAttribute (WidthAttribute a) = Attribute "width" (T.pack ("'" ++ show a ++ "'"))
   fromAttribute (Attribute "width" w) = case readMaybe (T.unpack w) of
                                    (Just w') -> Right (WidthAttribute w')
                                    Nothing -> Left $ T.concat ["WidthAttribute value not parsing -->",w]
   fromAttribute (Attribute other _) = wrongAttrResponse "width" other
