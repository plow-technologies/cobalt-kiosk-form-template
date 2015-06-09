{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Kiosk.Backend.Form.Element.Logo( Logo(..)
                                      , LogoAttributes (..)
                                      , defaultLogo) where
import qualified Data.Text                         as T
import           GHC.Generics
import           Kiosk.Backend.Form.Attribute
import           Kiosk.Backend.Form.Attribute.Path

-- A Logo mainly is the Name of the Logo
data Logo = Logo {
  _getLogoText :: T.Text,
  _logoAttrib  :: [LogoAttributes]
} deriving (Show,Generic)

data LogoAttributes = LogoPath PathAttribute deriving (Show, Ord, Eq,Generic)

instance AttributeClass LogoAttributes where
   toAttribute (LogoPath a) = toAttribute a
   fromAttribute (Attribute "path" i) = Right $ LogoPath $ PathAttribute i
   fromAttribute _ = Left "Not a valid button Attribute"

defaultLogo :: Logo
defaultLogo = Logo "" [LogoPath . PathAttribute $ "Cobalt.png"]
