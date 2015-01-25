{- |
Module      :  Kiosk.Backend.Attribute
Description :  Attribute Types for the Element
Copyright   :  (c) <Plow Technology 2014>
License     :  <MIT>
Maintainer  :  <lingpo.huang@plowtech.net>
Stability   :  unstable
Portability :  portable
<Different Types of Attributes that are contain in the element>
-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}

module Kiosk.Backend.Form.Attribute ( AttributeClass(..)
                                    , Attribute(..)
                                    , wrongAttrResponse) where


import           Data.Aeson             (FromJSON, ToJSON)
import           Data.Either.Validation (Validation (..))
import           Data.Monoid            ((<>))

import           GHC.Generics           (Generic)

import           Data.Text              (Text)
-- Attributes
data Attribute = Attribute {
      name :: Text,
      val  :: Text
} deriving (Generic, Show)

instance ToJSON Attribute where
instance FromJSON Attribute where



-- Type Class for Attributes
class AttributeClass a where
  toAttribute :: a -> Attribute
  fromAttribute :: Attribute -> Validation Text a

-- | Utility Function
wrongAttrResponse :: Text -> Text -> Validation Text a
wrongAttrResponse correct other = Failure $ "Not " <> correct <> "attribute, recieved --> "  <> other

