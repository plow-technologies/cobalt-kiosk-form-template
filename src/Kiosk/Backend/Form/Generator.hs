
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}  

{- |
Module      :  Kiosk.Backend.Form.Generator
Description :  Generate Forms for current customers
Copyright   :  Plow Technologies LLC 
License     :  MIT License

Maintainer  :  Scott Murphy 
Stability   :  experimental
Portability :  portable

Generate various forms for our customers.
         
Try and keep as much generation data in our DB as possible.

         

-}

module Kiosk.Backend.Form.Generator () where 

import Kiosk.Backend.Form

import Network.Wreq (Response
                    ,post)
import Data.Aeson

-- import Data.Traversable
import Data.Monoid ((<>))   

import Data.Text (pack
                 ,Text)
                 
import Data.String (IsString)
import Data.ByteString.Lazy (ByteString)
