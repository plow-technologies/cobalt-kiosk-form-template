{-# LANGUAGE OverloadedStrings  #-}

module Kiosk.Backend.Form.Element.Logo( Logo(..)
                                      , LogoAttributes (..)
                                      , defaultLogo) where 

import Kiosk.Backend.Form.Attribute
import Kiosk.Backend.Form.Attribute.Path
import qualified Data.Text as T

-- A Logo mainly is the Name of the Logo
data Logo = Logo {
  _getLogoText :: T.Text,
  _logoAttrib  :: [LogoAttributes]
} deriving (Show)

data LogoAttributes = LogoPath PathAttribute deriving (Show, Ord, Eq)

instance AttributeClass LogoAttributes where
   toAttribute (LogoPath a) = toAttribute a
   fromAttribute (Attribute "path" i) = Right $ LogoPath $ PathAttribute i
   fromAttribute _ = Left "Not a valid button Attribute"

defaultLogo :: Logo
defaultLogo = Logo "" [LogoPath . PathAttribute $ "'Cobalt.png'"]