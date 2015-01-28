{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}


{- |
Module      :  Kiosk.Backend.Form.Attribute.Indexable
Description :  Indexable Attribute, True or false... Determines whether or not table will index this
Copyright   :  Plow Technologies LLC 
License     :  MIT License

Maintainer  :  Scott Murphy
Stability   :  experimental
Portability :  portable

-}


module Kiosk.Backend.Form.Attribute.Indexable (IndexableAttribute(..)) where
import Kiosk.Backend.Form.Attribute
import           Data.Aeson             (FromJSON, ToJSON)
import Language.Haskell.TH (nameBase)
import Data.Monoid ((<>))
import Data.Text (pack
                 ,unpack)
import           Text.Read              (readMaybe)                 
import           GHC.Generics           (Generic)
import Data.Either.Validation (Validation(..))

-- Indexable Attribute

data IndexableAttribute = IndexableAttribute {
                          _getIndexable::Bool
} deriving (Generic, Show, Ord, Eq)

instance ToJSON IndexableAttribute where
instance FromJSON IndexableAttribute where

instance AttributeClass IndexableAttribute where
   toAttribute (IndexableAttribute a) = Attribute "indexable" (pack ("'" ++ show a ++"'"))
   fromAttribute (Attribute "indexable" w) = case readMaybe (unpack w) of
                                        (Just w') -> Success . IndexableAttribute $ w'
                                        Nothing -> Failure $ (pack .nameBase $ 'IndexableAttribute) <>  "value not parsing -->" 
                                                                  <> w
   fromAttribute (Attribute other _) = wrongAttrResponse (pack.nameBase $ 'IndexableAttribute) other
