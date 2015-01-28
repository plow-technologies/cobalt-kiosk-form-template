{- |
Module      :  Kiosk.Backend.Element.Logo
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

module Kiosk.Backend.Form.Element.Logo ( Logo(..)
                                          , LogoAttributes (..)
                                          , defaultLogo) where 

import           Data.Aeson                   (FromJSON, ToJSON)
import           Data.Typeable                (Typeable)
import           GHC.Generics                 (Generic)
import           Control.Applicative    ((<$>), (<|>))
import           Data.Either.Validation (Validation(..))
import           Data.Text                    (Text)  
import           Kiosk.Backend.Form.Attribute.Path
import           Kiosk.Backend.Form.Attribute
-- A Logo mainly is the Name of the Logo
data Logo = Logo {
               _getLogoText :: Text,
               _logoAttrib  :: [LogoAttributes]
                      } deriving (Generic, Show, Ord, Eq, Typeable)

instance ToJSON Logo where
instance FromJSON Logo where


-- Logo Attributes
data LogoAttributes = LogoPath PathAttribute
                               deriving (Generic, Show, Ord, Eq)

instance ToJSON LogoAttributes where
instance FromJSON LogoAttributes where

instance AttributeClass LogoAttributes where
   toAttribute (LogoPath a) = toAttribute a
   fromAttribute  = tryAllLogoAttributes
     where
       tryAllLogoAttributes a' = LogoPath <$> fromAttribute a' <|> Failure "Not a valid Logo Attirbute"


defaultLogo :: Logo
defaultLogo = Logo "" [LogoPath . PathAttribute $ "'Cobalt.png'"  ]
