{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}


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
import qualified Data.Text                              as T
import           Kiosk.Backend.Form.Attribute
import           Kiosk.Backend.Form.Attribute.Width
import           Kiosk.Backend.Form.Element.Item.Input
import           Kiosk.Backend.Form.Element.Item.Option
import           Text.Read                              (readMaybe)
data Dropdown = Dropdown {
                           _getDropdown        :: Label
                         , _getDropdownOptions :: [Option]
                         , _getSubstituteInput :: Maybe Input}
 deriving (Generic,Show)



instance ToJSON Dropdown where
instance FromJSON Dropdown where

defaultDropdown :: Dropdown
defaultDropdown = Dropdown defaultLabel [defaultOption] Nothing


data DropdownAttributes = DropdownWidth WidthAttribute
  deriving (Show,Generic)


instance AttributeClass DropdownAttributes where
   toAttribute (DropdownWidth w) = toAttribute w
   fromAttribute (Attribute "width" w) = case readMaybe (T.unpack w) of
                                              (Just w') -> Right (DropdownWidth $ WidthAttribute w')
                                              Nothing -> Left $ T.concat ["DropdownAttribute value not parsing -->",w]
   fromAttribute _ = Left "Not a valid dropdown attribute"
