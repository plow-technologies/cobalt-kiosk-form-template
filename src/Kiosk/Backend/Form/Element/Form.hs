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


{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Kiosk.Backend.Form.Element.Form ( Form (..)
                                       , defaultForm) where

import           Data.Aeson                          (FromJSON, ToJSON)

import           GHC.Generics                        (Generic)
import           Kiosk.Backend.Form.Element.Address  (Address, defaultAddress)
import           Kiosk.Backend.Form.Element.Company  (Company, defaultCompany)
import           Kiosk.Backend.Form.Element.Constant (Constant, defaultConstant)
import           Kiosk.Backend.Form.Element.Logo     (Logo, defaultLogo)
import           Kiosk.Backend.Form.Element.Phone    (Phone, defaultPhone)
import           Kiosk.Backend.Form.Element.Row

-- A form is a list of Rows
data Form = Form {
    _getCompany   :: Company
  , _getAddress   :: Address
  , _getLogo      :: Logo
  , _getPhone     :: Phone
  , _getConstants :: [Constant]
  , _row          :: [Row]
 } deriving (Generic, Show)

instance ToJSON Form where
instance FromJSON Form where




defaultForm :: Form
defaultForm = Form defaultCompany defaultAddress defaultLogo defaultPhone [defaultConstant] defaultRows
