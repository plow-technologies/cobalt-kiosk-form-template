{-# LANGUAGE DeriveGeneric #-}
module Kiosk.Backend.Form.Element.Item.EmptyBlock (EmptyBlock(..)) where

import           Data.Aeson   (FromJSON, ToJSON)
import           GHC.Generics (Generic)


-- Empty Block
data EmptyBlock = Null deriving (Generic, Show)

instance ToJSON EmptyBlock where
instance FromJSON EmptyBlock where
