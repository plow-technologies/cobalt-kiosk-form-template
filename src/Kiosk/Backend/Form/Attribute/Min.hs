{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}



{- |
Module      :  Kiosk.Backend.Form.Attribute.Min
Description :  Min Attribute, determine min of something
Copyright   :  Plow Technologies LLC 
License     :  MIT License

Maintainer  :  Scott Murphy
Stability   :  experimental
Portability :  portable



-}


module Kiosk.Backend.Form.Attribute.Min ( MinAttributeDouble(..)
                                         ) where

import Kiosk.Backend.Form.Attribute

import Data.Text (pack
                 ,Text
                 ,unpack)

import Control.Applicative ((<$>))                                            
import Data.Monoid ((<>))
import           GHC.Generics           (Generic)

import           Data.Aeson             (FromJSON, ToJSON)

import Text.Read (readMaybe)

import Data.Either.Validation (Validation(..))


data MinAttributeDouble = MinAttributeDouble { _getGenericMinAmt :: Double }
    deriving (Generic,Show,Eq,Ord)

instance   ToJSON MinAttributeDouble where 
instance    FromJSON MinAttributeDouble where 





instance AttributeClass MinAttributeDouble where
  toAttribute (MinAttributeDouble d) = Attribute "mind" toVal
     where toVal = "'" <> (pack.show $ d) <> "'"
  fromAttribute (Attribute "mind" txtInteger) = convertToValidation $  
                                                       MinAttributeDouble <$> 
                                                                        (readMaybe.unpack $ txtInteger)


  fromAttribute (Attribute other _) = wrongAttrResponse "mind" other                                                            
-- convertToValidation :: (Num a)=>  (Maybe (MinAttribute a) ) -> (Validation Text (MinAttribute a))

convertToValidation :: Maybe a -> Validation Text a
convertToValidation Nothing = Failure ("Incorrect Number Type"::Text)
convertToValidation (Just i) = Success i                     
