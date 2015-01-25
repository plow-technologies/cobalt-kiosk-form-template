{-# LANGUAGE DeriveGeneric #-}


{- |
Module      :  Kiosk.Backend.Form.Element.Item.EmptyBlock
Description :  EmptyBlock Element
Copyright   :  Plow Technologies LLC 
License     :  MIT License

Maintainer  :  Scott Murphy
Stability   :  experimental
Portability :  portable



-}

module Kiosk.Backend.Form.Element.Item.EmptyBlock (EmptyBlock(..)) where
import Data.Aeson (ToJSON
                  ,FromJSON)
import GHC.Generics (Generic)
import Data.Text (Text)
        
-- Empty Block
data EmptyBlock = Null deriving (Generic, Show)

instance ToJSON EmptyBlock where
instance FromJSON EmptyBlock where
                
