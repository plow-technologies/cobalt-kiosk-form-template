{-# LANGUAGE DeriveGeneric #-}
module Kiosk.Backend.Form.Element.Item.EmptyBlock (EmptyBlock(..)) where

import GHC.Generics (Generic)

-- Empty Block
data EmptyBlock = Null deriving (Generic, Show)