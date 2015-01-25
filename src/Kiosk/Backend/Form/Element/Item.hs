{- |
Module      :  Kiosk.Backend.Element.Item
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

module Kiosk.Backend.Form.Element.Item ( Item(..)
                                       , ItemType (..)
                                       , defaultItem
                                       , defaultRadioItem) where 

import           Data.Aeson                   (FromJSON, ToJSON)

import           GHC.Generics                 (Generic)
import           Control.Applicative    ((<$>), (<|>))
import           Data.Either.Validation (Validation(..))
import           Kiosk.Backend.Form.Element.Item.TableLeftHeader 
import           Kiosk.Backend.Form.Element.Item.TableTopHeader
import           Kiosk.Backend.Form.Element.Item.EmptyBlock
import           Kiosk.Backend.Form.Element.Item.Radio
import           Kiosk.Backend.Form.Element.Item.Button
import           Kiosk.Backend.Form.Element.Item.Label
import           Kiosk.Backend.Form.Element.Item.Input
import           Kiosk.Backend.Form.Attribute.Width
import           Kiosk.Backend.Form.Attribute

-- A Item containing different item type and its attirbutes
data Item = Item {
  _item       :: [ItemType],
  _itemAttrib :: [ItemAttributes]
} deriving (Generic, Show)

instance ToJSON Item where
instance FromJSON Item where


-- Item Attributes
data ItemAttributes = ItemWidth WidthAttribute
                           deriving (Generic, Show)

instance ToJSON ItemAttributes where
instance FromJSON ItemAttributes where

instance AttributeClass ItemAttributes where
   toAttribute (ItemWidth a) = toAttribute a
   fromAttribute  = tryAllItemAttributes
     where
       tryAllItemAttributes a' = ItemWidth <$> fromAttribute a' <|> Failure "Not a valid Row Attirbute"


-- A Item type can be a label, input, emptyBlock, tableTopHeader, tableLeftHeader
data ItemType = ItemLabel Label
               |ItemInput Input
               |ItemButton Button
               |ItemRadio Radio 
               |ItemEmptyBlock EmptyBlock
               |ItemTableTopHeader TableTopHeader
               |ItemTableLeftHeader TableLeftHeader                
           deriving (Generic, Show)

instance ToJSON ItemType where
instance FromJSON ItemType where

defaultItem :: Item
defaultItem = defaultInputItem

defaultInputItem :: Item
defaultInputItem = Item [ItemLabel defaultLabel, ItemInput defaultInput] [ItemWidth $ WidthAttribute (12::Int)]

defaultRadioItem :: Item
defaultRadioItem = Item [ItemRadio defaultRadio] []
