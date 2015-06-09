

{- |
Module      :  Kiosk.Backend.Form.Element.Item.Dropdown
Description :  Dropdown menu for kiosk project
Copyright   :  Plow Technologies LLC
License     :  MIT License

Maintainer  :  Scott Murphy
Stability   :  experimental
Portability :  portable

Dropdown button with text input

-}


module Kiosk.Backend.Form.Element.Item.Dropdown where

import           Kiosk.Backend.Form.Element.Item.Label  (Label (..),
                                                         defaultLabel)

import           Kiosk.Backend.Form.Element.Item.Option

data Dropdown = Dropdown {
 _getDropdown :: Label}
