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

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}

module Kiosk.Backend.Form.Element.Form ( Form (..)
                                       , defaultForm) where 

import           Data.Aeson                   (FromJSON, ToJSON)
import           Data.Typeable                (Typeable)
import           GHC.Generics                 (Generic)
import           Control.Applicative    ((<$>), (<|>))
import           Data.Either.Validation (Validation(..))
import           Data.Text                    (Text)  
import           Kiosk.Backend.Form.Attribute.Width
import           Kiosk.Backend.Form.Attribute
import           Kiosk.Backend.Form.Element.Company 
import           Kiosk.Backend.Form.Element.Address 

import Kiosk.Backend.Form.Element.Row 

-- A form is a list of Rows
data Form = Form {
  _getCompany :: Company,
  _getAddress :: Address,
  _row        :: [Row]
 } deriving (Generic, Show)

instance ToJSON Form where
instance FromJSON Form where




defaultForm :: Form
defaultForm = Form defaultCompany defaultAddress [defaultRow]       
