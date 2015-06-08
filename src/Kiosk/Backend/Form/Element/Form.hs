{-# LANGUAGE DeriveGeneric     #-}
{- |
Module      :  Kiosk.Backend.Element.Form
Description :  Form Element Types for Kiosk
Copyright   :  (c) <Plow Technology 2014>
License     :  <MIT>
Maintainer  :  <lingpo.huang@plowtech.net>
Stability   :  unstable
Portability :  portable
<Haskell Types for the form sending from the SWIF Application>
-}

{-# LANGUAGE OverloadedStrings #-}

module Kiosk.Backend.Form.Element.Form ( Form (..)
                                       , defaultForm) where

import           Data.Aeson                          (FromJSON, ToJSON)
import           GHC.Generics
import           Kiosk.Backend.Form.Element.Address
import           Kiosk.Backend.Form.Element.Company
import           Kiosk.Backend.Form.Element.Constant
import           Kiosk.Backend.Form.Element.Logo
import           Kiosk.Backend.Form.Element.Phone
import           Kiosk.Backend.Form.Element.Row
-- A form is a list of Rows
data Form = Form {
    _getCompany   :: Company
  , _getAddress   :: Address
  , _getLogo      :: Logo
  , _getPhone     :: Phone
  , _getConstants :: [Constant]
  , _row          :: [Row]
} deriving (Show,Generic)


instance ToJSON Form where
instance FromJSON Form where

defaultForm :: Form
defaultForm = Form defaultCompany defaultAddress defaultLogo defaultPhone [defaultConstant] defaultRows
