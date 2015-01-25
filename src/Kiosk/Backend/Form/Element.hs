{- |
Module      :  Kiosk.Backend.Element
Description :  Form Element Types for Kiosk
Copyright   :  (c) <Plow Technology 2014>
License     :  <MIT>
Maintainer  :  <lingpo.huang@plowtech.net>
Stability   :  unstable
Portability :  portable
<Haskell Types for the form sending from the SWIF Application>
         
It can be confusing looking for Elements and Attributes,
   
The rule is Every element has a list of attributes defined along side it that 
define what attributes can be used with that element but the primitive types 
of these attribute definitions come from the Attribute type.
   
For example, 'Company' has 'CompanyAttribute', one of which is a 'WidthAttr'   
-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}

module Kiosk.Backend.Form.Element (  module Kiosk.Backend.Form.Element
                                   , module Kiosk.Backend.Form.Attribute) where

import           Data.Aeson                   (FromJSON, ToJSON)
import           Data.Text                    (Text)
import           Data.Typeable                (Typeable)
import           GHC.Generics                 (Generic)
import           Kiosk.Backend.Form.Attribute
-- import           Kiosk.Backend.Form.Element.Company (Company)
-- A form is a list of Rows
data Form = Form {
  _getCompany :: Company,
  _getAddress :: Address,
  _row        :: [Row]
 } deriving (Generic, Show)

instance ToJSON Form where
instance FromJSON Form where



instance ToJSON Address where
instance FromJSON Address where
-- A row containing a list of items and its attributes
data Row = Row {
 _rowItem   :: [Item],
 _rowAttrib :: [RowAttributes]
} deriving (Generic, Show)

instance ToJSON Row where
instance FromJSON Row where

-- A Item containing different item type and its attirbutes
data Item = Item {
  _item       :: [ItemType],
  _itemAttrib :: [ItemAttributes]
} deriving (Generic, Show)

instance ToJSON Item where
instance FromJSON Item where

-- A Item type can be a label, input, emptyBlock, tableTopHeader, tableLeftHeader
data ItemType = ItemLabel Label
               |ItemInput Input
               |ItemButton Button
               |ItemEmptyBlock EmptyBlock
               |ItemTableTopHeader TableTopHeader
               |ItemTableLeftHeader TableLeftHeader deriving (Generic, Show)

instance ToJSON ItemType where
instance FromJSON ItemType where

-- A label is a text with set of attributes
data Label = Label {
                    _getLabelText :: Text,
                    _labelAttrib  :: [LabelAttributes]
                    } deriving (Generic, Show)

instance ToJSON Label where
instance FromJSON Label where

-- Empty Block
data EmptyBlock = Null deriving (Generic, Show)

instance ToJSON EmptyBlock where
instance FromJSON EmptyBlock where

-- Header of table on top
data TableTopHeader = TableTopHeader {
                    _getTableTopHeader :: Text
                                     } deriving (Generic, Show)

instance ToJSON TableTopHeader where
instance FromJSON TableTopHeader where

-- Header of table on Left
data TableLeftHeader = TableLeftHeader {
                    _getTableLeftHeader :: Text
                                     } deriving (Generic, Show)

instance ToJSON TableLeftHeader where
instance FromJSON TableLeftHeader where

-- Button is Text with set of attributes
data Button = Button {
   _getButtonText :: Text,
   _buttonAttrib  :: [ButtonAttributes]
} deriving (Generic, Show)

instance ToJSON Button where
instance FromJSON Button where


defaultCompany :: Company
defaultCompany = Company "Hull's Oilfield LLC" [CompanyWidth $ WidthAttr (12::Int) ]

defaultAddress :: Address
defaultAddress = Address "PO Box 130 Wilson, Oklahoma 73463\n886-849-5483\nAnswering Service 580-220-9936" [AddressWidth $ WidthAttr (12::Int)]

defaultInput :: Input
defaultInput = Input defaultInputType defaultInputAttributesList

defaultInputType :: InputType
defaultInputType = InputTypeText $ InputText (""::Text)

defaultInputAttributesList :: [InputAttribute]
defaultInputAttributesList = [wAttr, tAttr]
              where wAttr = InputWidth $ WidthAttr (12::Int)
                    tAttr = InputType $ InputTypeAttr defaultInputType

defaultLabel :: Label
defaultLabel = Label "Legal Dest" [LabelWidth $ WidthAttr (12::Int)]

defaultItem :: Item
defaultItem = Item [ItemLabel defaultLabel, ItemInput defaultInput] [ItemWidth $ WidthAttr (12::Int)]

defaultRow :: Row
defaultRow = Row [defaultItem] [RowWidth $ WidthAttr (12::Int)]

defaultForm :: Form
defaultForm = Form defaultCompany defaultAddress [defaultRow]
