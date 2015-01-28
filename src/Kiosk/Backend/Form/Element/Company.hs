{- |
Module      :  Kiosk.Backend.Element.Company
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

module Kiosk.Backend.Form.Element.Company ( Company(..)
                                          , CompanyAttributes (..)
                                          , defaultCompany) where 

import           Data.Aeson                   (FromJSON, ToJSON)
import           Data.Typeable                (Typeable)
import           GHC.Generics                 (Generic)
import           Control.Applicative    ((<$>), (<|>))
import           Data.Either.Validation (Validation(..))
import           Data.Text                    (Text)  
import           Kiosk.Backend.Form.Attribute.Width
import           Kiosk.Backend.Form.Attribute
-- A Company mainly is the Name of the Company
data Company = Company {
               _getCompanyText :: Text,
               _companyAttrib  :: [CompanyAttributes]
                       } deriving (Generic, Show, Ord, Eq, Typeable)

instance ToJSON Company where
instance FromJSON Company where


-- Company Attributes
data CompanyAttributes = CompanyWidth WidthAttribute
                           deriving (Generic, Show, Ord, Eq)

instance ToJSON CompanyAttributes where
instance FromJSON CompanyAttributes where

instance AttributeClass CompanyAttributes where
   toAttribute (CompanyWidth a) = toAttribute a
   fromAttribute  = tryAllCompanyAttributes
     where
       tryAllCompanyAttributes a' = CompanyWidth <$> fromAttribute a' <|> Failure "Not a valid Company Attirbute"


defaultCompany :: Company
defaultCompany = Company "Hull's Oilfield LLC" [CompanyWidth $ WidthAttribute (12::Int) ]
