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



module Kiosk.Backend.Form.Converter (convertInputIfPossible) where

import           Kiosk.Backend.Form.Element.Item.Input (InputDate (..),
                                                        InputText (..),
                                                        InputType (..),
                                                        Signature (..),
                                                        makeInputDate)

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

convertInputIfPossible :: InputType -> InputType -> InputType
convertInputIfPossible (InputTypeSignature insignature) (InputTypeText intext) = InputTypeSignature .
                                                                                 convertToSignature insignature $ intext
convertInputIfPossible (InputTypeDate indate) (InputTypeText intext) = either InputTypeText InputTypeDate $ convertToDate indate intext
convertInputIfPossible _ t = t

convertToSignature :: Signature -> InputText -> Signature
convertToSignature _ (InputText s) = Signature s

-- |SILENT fail here, the input date will just not be converted
-- I am thinking this will help with backwards compatability
convertToDate :: InputDate -> InputText -> Either InputText InputDate
convertToDate id itxt@(InputText t) =  either (const $ Left itxt)
                                              Right
                                              (makeInputDate t)




