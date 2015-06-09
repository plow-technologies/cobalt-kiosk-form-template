{-# LANGUAGE DeriveGeneric #-}


{- |
Module      :  Kiosk.Backend.Form.Element.Item.Dropdown
Description :  Dropdown menu for kiosk project
Copyright   :  Plow Technologies LLC
License     :  MIT License

Maintainer  :  Scott Murphy
Stability   :  experimental
Portability :  portable

Dropdown button with text input
Single Select
-}


module Kiosk.Backend.Form.Element.Item.Dropdown where
import           GHC.Generics
import           Kiosk.Backend.Form.Element.Item.Label  (Label (..),
                                                         defaultLabel)

import           Data.Aeson                             (FromJSON, ToJSON)
import           Kiosk.Backend.Form.Element.Item.Input
import           Kiosk.Backend.Form.Element.Item.Option

data Dropdown = Dropdown {
                           _getDropdown        :: Label
                         , _getDropdownOptions :: [Option]
                         , _getSubstituteInput :: Maybe Input}
 deriving (Generic,Show)



instance ToJSON Dropdown where
instance FromJSON Dropdown where

defaultDropdown :: Dropdown
defaultDropdown = Dropdown defaultLabel [defaultOption] Nothing
