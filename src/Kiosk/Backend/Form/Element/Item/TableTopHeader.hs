{-# LANGUAGE DeriveGeneric #-}

module Kiosk.Backend.Form.Element.Item.TableTopHeader (TableTopHeader(..)) where
import           Data.Aeson   (FromJSON, ToJSON)
import qualified Data.Text    as T
import           GHC.Generics (Generic)
       -- Header of table on top
data TableTopHeader = TableTopHeader {
  _getTableTopHeader :: T.Text
} deriving (Generic, Show)

instance ToJSON TableTopHeader where
instance FromJSON TableTopHeader where
