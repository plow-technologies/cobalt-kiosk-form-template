{- |
Module      :  Kiosk.Backend.Form.Converter
Description :  Convert Between Form Types
Copyright   :  (c) <Plow Technology 2015>
License     :  <MIT>
Maintainer  :  <scott.murphy@plowtech.net>
Stability   :  unstable
Portability :  portable


Convert one form InputType into another, if possible.  Otherwise fail.

|-}



module Kiosk.Backend.Form.Converter () where

import           Kiosk.Backend.Form.Element.Item.Input (InputDate (..),
                                                        InputText (..),
                                                        InputType (..),
                                                        Signature (..),
                                                        csvDateStd)

{-|
convert an input into a given input if possible.

if not, return original input.


data InputType = InputTypeText InputText
                |InputTypeSignature Signature
                |InputTypeInt InputInt
                |InputTypeDate InputDate
                |InputTypeTime InputTime
                |InputTypeDouble InputDouble deriving (Generic, Show, Ord, Eq, Typeable)


|-}

convertInputIfPossible (InputTypeSignature insignature) (InputTypeText intext) = InputTypeSignature .
                                                                                 convertToSignature insignature $ intext
convertInputIfPossible (InputTypeDate indate) (InputTypeText intext) = InputTypeDate . convertToDate indate $ intext

convertToSignature :: Signature -> InputText -> Signature
convertToSignature _ (InputText s) = Signature s

convertToDate :: InputDate -> InputText -> InputDate
convertToDate = undefined




