{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Kiosk.Backend.Form.Element.Company( Company(..)
                                         , CompanyAttributes (..)
                                         , defaultCompany) where

import qualified Data.Text                          as T
import           GHC.Generics
import           Kiosk.Backend.Form.Attribute
import           Kiosk.Backend.Form.Attribute.Width
import           Text.Read                          (readMaybe)

-- A Company contain address information of the Company
data Company = Company {
  _getCompanyText :: T.Text,
  _companyAttrib  :: [CompanyAttributes]
} deriving (Show, Ord, Eq,Generic)

data CompanyAttributes = CompanyWidth WidthAttribute deriving (Show, Ord, Eq,Generic)


instance AttributeClass CompanyAttributes where
  toAttribute (CompanyWidth a) = toAttribute a
  fromAttribute (Attribute "width" w) = case readMaybe (T.unpack w) of
                                    (Just w') -> Right (CompanyWidth (WidthAttribute w'))
                                    Nothing -> Left $ T.concat ["CompanyAttributes value not parsing -->",w]
  fromAttribute (Attribute other _) = wrongAttrResponse "width" other

defaultCompany :: Company
defaultCompany = Company "Hull's Oilfield LLC" [CompanyWidth $ WidthAttribute (12::Int) ]
