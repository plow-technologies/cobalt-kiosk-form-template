{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{- |
Module      :  Kiosk.Backend.Form.Element.Constant
Description :  Constant Interpolation from server side
Copyright   :  Plow Technologies LLC 
License     :  MIT License

Maintainer  :  Constant Interpolation from server side
Stability   :  experimental
Portability :  portable

-}

module Kiosk.Backend.Form.Element.Constant ( Constant(..)
                                            , ConstantAttributes(..)
                                            , defaultConstant) where

import Data.Aeson ( ToJSON
                  , FromJSON )
                  
import GHC.Generics (Generic)

import Data.Text (Text)

import Kiosk.Backend.Form.Attribute (AttributeClass(..)
                                    ,Attribute(..))

import           Data.Either.Validation (Validation(..))
import Kiosk.Backend.Form.Attribute.Indexable

-- Button is Text with set of attributes
data Constant = Constant {
   _getConstantText :: Text,
   _constantAttrib  :: [ConstantAttributes]
} deriving (Generic, Show)

instance ToJSON Constant where
instance FromJSON Constant where



-- Button Atrributes
data ConstantAttributes = ConstantAttributeType Text |ConstantAttributeIndexable IndexableAttribute
  deriving (Generic, Show)

instance ToJSON ConstantAttributes where
instance FromJSON ConstantAttributes where

instance AttributeClass ConstantAttributes where
   toAttribute (ConstantAttributeType t) = Attribute "type" t
   toAttribute (ConstantAttributeIndexable i) = toAttribute i
   fromAttribute  = tryAllCompanyAttributes
     where
       tryAllCompanyAttributes (Attribute "type" i) =  Success . ConstantAttributeType $ i 
       tryAllCompanyAttributes _ =  Failure "Not a valid button Attirbute"


defaultConstant :: Constant
defaultConstant = Constant "Black Watch" [ ConstantAttributeType "'Company'"
                                         , ConstantAttributeIndexable $ IndexableAttribute True ]
