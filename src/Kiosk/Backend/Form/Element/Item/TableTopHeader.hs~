{-# LANGUAGE DeriveGeneric #-}


{- |
Module      :  Kiosk.Backend.Form.Element.Item.TableLeftHeader
Description :  Left Header Element
Copyright   :  Plow Technologies LLC 
License     :  MIT License

Maintainer  :  Scott Murphy
Stability   :  experimental
Portability :  portable

|-}
        
module Kiosk.Backend.Form.Element.Item.TableLeftHeader (TableLeftHeader(..)) where        

import Data.Aeson (ToJSON
                  ,FromJSON)
import GHC.Generics (Generic)
import Data.Text (Text)
-- Header of table on Left
data TableLeftHeader = TableLeftHeader {
                    _getTableLeftHeader :: Text
                                     } deriving (Generic, Show)

instance ToJSON TableLeftHeader where
instance FromJSON TableLeftHeader where
