{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}


{- |
Module      :  Kiosk.Backend.Form.Attribute.Max
Description :  Max Attribute, determine max of something
Copyright   :  Plow Technologies LLC 
License     :  MIT License

Maintainer  :  Scott Murphy
Stability   :  experimental
Portability :  portable



-}

        
module Kiosk.Backend.Form.Attribute.Max ( MaxAttribute
                                        , makeMaxAttribute) where

import Kiosk.Backend.Form.Attribute
import Data.Text (pack
                 ,Text
                 ,unpack)
import Control.Applicative ((<$>))                                            
import           GHC.Generics           (Generic)
import           Data.Aeson             (FromJSON, ToJSON)
import Text.Read (readMaybe)
import Data.Either.Validation (Validation(..))

data GenericMaxAttribute a = MaxAttribute { _getGenericMaxAmt :: a }
    deriving (Generic,Show)
instance (ToJSON a) =>  ToJSON (GenericMaxAttribute a) where 
instance (FromJSON a) =>  FromJSON (GenericMaxAttribute a) where 


type MaxAttribute a = (Num a) => GenericMaxAttribute a

makeMaxAttribute :: (Num a) => a -> MaxAttribute a
makeMaxAttribute = MaxAttribute



instance (Show a,Num a,Read a) =>  AttributeClass (GenericMaxAttribute a) where
  toAttribute (MaxAttribute a) = Attribute "max" (pack.show $ a)
  fromAttribute (Attribute "max" txtInteger) = convertToValidation $  
                                                    makeMaxAttribute <$> 
                                                                 (readMaybe.unpack $ txtInteger)


  fromAttribute (Attribute other _) = wrongAttrResponse "max" other                                                            
-- convertToValidation :: (Num a)=>  (Maybe (MaxAttribute a) ) -> (Validation Text (MaxAttribute a))

convertToValidation :: Maybe a -> Validation Text a
convertToValidation Nothing = Failure ("Incorrect Number Type"::Text)
convertToValidation (Just i) = Success i                     
