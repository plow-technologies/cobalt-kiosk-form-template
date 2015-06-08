{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}

{- |
Module      :  Kiosk.Backend.Form.Element.Item.AutoInput
Description :  Inputs provided by the rendering server
Copyright   :  Plow Technologies LLC
License     :  MIT License
Maintainer  :  Scott Murphy
Stability   :  experimental
Portability :  portable

Several items need to be provided as inputs by the rendering server
These are called '<auto-input>' labels but are in every other way
identical to other labels

-}


module Kiosk.Backend.Form.Element.Item.AutoInput (AutoInput(..)) where

import           Data.Aeson                            (FromJSON, ToJSON)
import           Data.Typeable                         (Typeable)
import           GHC.Generics                          (Generic)
import           Kiosk.Backend.Form.Element.Item.Input (Input)
newtype AutoInput = AutoInput{_getAutoInput :: Input}
  deriving (Generic,Show,Ord,Eq,Typeable)

instance ToJSON AutoInput where
instance FromJSON AutoInput where
