{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}



{- |
Module      :  Kiosk.Backend.Form.Attribute.Max
Description :  Max Attribute, determine max of something
Copyright   :  Plow Technologies LLC 
License     :  MIT License

Maintainer  :  Scott Murphy
Stability   :  experimental
Portability :  portable



-}

        
module Kiosk.Backend.Form.Attribute.Path ( PathAttribute(..)
                                          ) where

import Kiosk.Backend.Form.Attribute

import Data.Text (Text)


import           GHC.Generics           (Generic)

import           Data.Aeson             (FromJSON, ToJSON)

import Data.Either.Validation (Validation(..))


data PathAttribute = PathAttribute { _getPath :: Text }
    deriving (Generic,Show,Eq,Ord)

instance   ToJSON PathAttribute where 
instance    FromJSON PathAttribute where 





instance AttributeClass PathAttribute where
  toAttribute (PathAttribute d) = Attribute "path" d
  fromAttribute (Attribute "path" pathTxt) = Success . PathAttribute $ pathTxt
  fromAttribute (Attribute other _) = wrongAttrResponse "maxd" other                               
