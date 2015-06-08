{-# LANGUAGE DeriveGeneric #-}

module Kiosk.Backend.Form.Element.Item.TableLeftHeader (TableLeftHeader(..)) where
import           Data.Aeson   (FromJSON, ToJSON)
import qualified Data.Text    as T
import           GHC.Generics (Generic)
-- Header of table on Left
data TableLeftHeader = TableLeftHeader {
	_getTableLeftHeader :: T.Text
} deriving (Generic, Show)

instance ToJSON TableLeftHeader where
instance FromJSON TableLeftHeader where
