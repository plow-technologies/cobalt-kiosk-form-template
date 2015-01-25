{-# LANGUAGE DeriveGeneric #-}


{- |
Module      :  Kiosk.Backend.Form.Element.Item.TableTopHeader
Description :  Top Header Element
Copyright   :  Plow Technologies LLC 
License     :  MIT License

Maintainer  :  Scott Murphy
Stability   :  experimental
Portability :  portable

|-}
        
module Kiosk.Backend.Form.Element.Item.TableTopHeader (TableTopHeader(..)) where        

import Data.Aeson (ToJSON
                  ,FromJSON)
import GHC.Generics (Generic)
import Data.Text (Text)

 -- Header of table on top
data TableTopHeader = TableTopHeader {
                    _getTableTopHeader :: Text
                                     } deriving (Generic, Show)

instance ToJSON TableTopHeader where
instance FromJSON TableTopHeader where
