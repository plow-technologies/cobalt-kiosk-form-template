{- |
Module      :  Kiosk.Backend.Element.Row
Description :  Row Element Types for Kiosk
Copyright   :  (c) <Plow Technology 2014>
License     :  <MIT>
Maintainer  :  <lingpo.huang@plowtech.net>
Stability   :  unstable
Portability :  portable
<Haskell Types for the form sending from the SWIF Application>
-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}

module Kiosk.Backend.Form.Element.Row (Row(..)
                                      ,RowAttributes
                                      ,defaultRows) where 

import           Data.Aeson                   (FromJSON, ToJSON)
import           GHC.Generics                 (Generic)
import           Control.Applicative    ((<$>), (<|>))
import           Data.Either.Validation (Validation(..))
import           Kiosk.Backend.Form.Attribute.Width
import           Kiosk.Backend.Form.Attribute
import Kiosk.Backend.Form.Element.Item

-- A row containing a list of items and its attributes
data Row = Row {
 _rowItem   :: [Item],
 _rowAttrib :: [RowAttributes]
} deriving (Generic, Show)

instance ToJSON Row where
instance FromJSON Row where

-- Row Attributes
data RowAttributes = RowWidth WidthAttribute
                           deriving (Generic, Show)

instance ToJSON RowAttributes where
instance FromJSON RowAttributes where

instance AttributeClass RowAttributes where
   toAttribute (RowWidth a) = toAttribute a
   fromAttribute  = tryAllRowAttributes
     where
       tryAllRowAttributes a' = RowWidth <$> fromAttribute a' <|> Failure "Not a valid Row Attirbute"


defaultRows :: [Row]
defaultRows = [Row [defaultItem] [RowWidth $ WidthAttribute (12::Int)]
              ,Row [defaultRadioItem] [RowWidth $ WidthAttribute (12::Int)] ]
