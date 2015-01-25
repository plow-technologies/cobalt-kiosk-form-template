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

        
module Kiosk.Backend.Form.Attribute.Max ( MaxAttributeDouble(..)
                                         ) where

import Kiosk.Backend.Form.Attribute

import Data.Text (pack
                 ,Text
                 ,unpack)
                 
import Control.Applicative ((<$>))                                            

import           GHC.Generics           (Generic)

import           Data.Aeson             (FromJSON, ToJSON)

import Text.Read (readMaybe)

import Data.Either.Validation (Validation(..))


data MaxAttributeDouble = MaxAttributeDouble { _getGenericMaxAmt :: Double }
    deriving (Generic,Show,Eq,Ord)

instance   ToJSON MaxAttributeDouble where 
instance    FromJSON MaxAttributeDouble where 





instance AttributeClass MaxAttributeDouble where
  toAttribute (MaxAttributeDouble d) = Attribute "maxd" (pack.show $ d)
  fromAttribute (Attribute "maxd" txtInteger) = convertToValidation $  
                                                     MaxAttributeDouble <$> 
                                                                      (readMaybe.unpack $ txtInteger)


  fromAttribute (Attribute other _) = wrongAttrResponse "maxd" other                                                            
-- convertToValidation :: (Num a)=>  (Maybe (MaxAttribute a) ) -> (Validation Text (MaxAttribute a))

convertToValidation :: Maybe a -> Validation Text a
convertToValidation Nothing = Failure ("Incorrect Number Type"::Text)
convertToValidation (Just i) = Success i                     
