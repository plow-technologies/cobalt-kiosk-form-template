{-# LANGUAGE OverloadedStrings  #-}

module Kiosk.Backend.Form.Element.Form ( Form (..)
                                       , defaultForm) where

import Kiosk.Backend.Form.Element.Company
import Kiosk.Backend.Form.Element.Address
import Kiosk.Backend.Form.Element.Logo                                                    
import Kiosk.Backend.Form.Element.Phone
import Kiosk.Backend.Form.Element.Constant
import Kiosk.Backend.Form.Element.Row 

-- A form is a list of Rows
data Form = Form {
    _getCompany   :: Company
  , _getAddress   :: Address
  , _getLogo      :: Logo             
  , _getPhone     :: Phone             
  , _getConstants :: [Constant]              
  , _row          :: [Row]
} deriving (Show)

defaultForm :: Form
defaultForm = Form defaultCompany defaultAddress defaultLogo defaultPhone [defaultConstant] defaultRows   