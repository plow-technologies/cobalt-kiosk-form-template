{- |
Module      :  Kiosk.Backend.Attribute
Description :  Attribute Types for the Element
Copyright   :  (c) <Plow Technology 2014>
License     :  <MIT>
Maintainer  :  <lingpo.huang@plowtech.net>
Stability   :  unstable
Portability :  portable
<Different Types of Attributes that are contain in the element>
-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}

module Kiosk.Backend.Form.Attribute where

import           Control.Applicative    ((<$>), (<|>))
import           Data.Aeson             (FromJSON, ToJSON)
import           Data.Either.Validation
import           Data.Monoid            ((<>))
import           Data.Text              (Text, pack, unpack)
import           Data.Typeable          (Typeable)
import           GHC.Generics           (Generic)
import           Text.Read              (readMaybe)

-- Attributes
data Attribute = Attribute {
      name :: Text,
      val  :: Text
} deriving (Generic, Show)

instance ToJSON Attribute where
instance FromJSON Attribute where

-- Type Class for Attributes
class AttributeClass a where
  toAttribute :: a -> Attribute
  fromAttribute :: Attribute -> Validation Text a


-- Width Attribute
data WidthAttr = WidthAttr {
                    _getWidth::Int
} deriving (Generic, Show, Ord, Eq)

instance ToJSON WidthAttr where
instance FromJSON WidthAttr where

instance AttributeClass WidthAttr where
   toAttribute (WidthAttr a) = Attribute "width" (pack ("'" ++ show a ++"'"))
   fromAttribute (Attribute "width" w) = case readMaybe (unpack w) of
                                    (Just w') -> Success . WidthAttr $ w'
                                    Nothing -> Failure $ pack "WidthAttribute value not parsing -->" <> w
   fromAttribute (Attribute other _) = wrongAttrResponse "width" other

-- Type Attribute
data InputTypeAttr = InputTypeAttr {
   _getTypeName :: InputType
} deriving (Generic, Show, Ord, Eq)

instance ToJSON InputTypeAttr where
instance FromJSON InputTypeAttr where

instance AttributeClass InputTypeAttr where
   toAttribute (InputTypeAttr a) = toAttribute a
   fromAttribute (Attribute "type" v) = case v of
                                         "text" -> Success . InputTypeAttr . InputTypeText . InputText $  v
                                         "signature" -> Success . InputTypeAttr . InputTypeSignature . Signature $ v
                                         "int" -> Success . InputTypeAttr . InputTypeInt . InputInt $ 0
                                         "double" -> Success. InputTypeAttr . InputTypeDouble . InputDouble $ 0.0
                                         _ -> Failure $ pack "TypeAttribute value not parsing -->" <> v
   fromAttribute (Attribute other _) = wrongAttrResponse "type" other

-- Action Attribute
data ActionAttr = ActionAttr {
   _getFunctionName :: String
} deriving (Generic, Show)

instance ToJSON ActionAttr where
instance FromJSON ActionAttr where

instance AttributeClass ActionAttr where
   toAttribute (ActionAttr a) = Attribute "action" (pack.show $ a)
   fromAttribute (Attribute "action" v) = Success. ActionAttr . unpack $ v
   fromAttribute (Attribute other _) = wrongAttrResponse "action" other

-- | Attributes warpper type

-- Company Attributes
data CompanyAttributes = CompanyWidth WidthAttr
                           deriving (Generic, Show, Ord, Eq)

instance ToJSON CompanyAttributes where
instance FromJSON CompanyAttributes where

instance AttributeClass CompanyAttributes where
   toAttribute (CompanyWidth a) = toAttribute a
   fromAttribute  = tryAllCompanyAttributes
     where
       tryAllCompanyAttributes a' = CompanyWidth <$> fromAttribute a' <|> Failure "Not a valid Company Attirbute"

-- Address Attributes
data AddressAttributes = AddressWidth WidthAttr
                           deriving (Generic, Show, Ord, Eq)


instance ToJSON AddressAttributes where
instance FromJSON AddressAttributes where

instance AttributeClass AddressAttributes where
   toAttribute (AddressWidth a) = toAttribute a
   fromAttribute  = tryAllAddressAttributes
     where
       tryAllAddressAttributes a' = AddressWidth <$> fromAttribute a' <|> Failure "Not a valid Address Attirbute"

-- Row Attributes
data RowAttributes = RowWidth WidthAttr
                           deriving (Generic, Show)

instance ToJSON RowAttributes where
instance FromJSON RowAttributes where

instance AttributeClass RowAttributes where
   toAttribute (RowWidth a) = toAttribute a
   fromAttribute  = tryAllRowAttributes
     where
       tryAllRowAttributes a' = RowWidth <$> fromAttribute a' <|> Failure "Not a valid Row Attirbute"

-- Item Attributes
data ItemAttributes = ItemWidth WidthAttr
                           deriving (Generic, Show)

instance ToJSON ItemAttributes where
instance FromJSON ItemAttributes where

instance AttributeClass ItemAttributes where
   toAttribute (ItemWidth a) = toAttribute a
   fromAttribute  = tryAllItemAttributes
     where
       tryAllItemAttributes a' = ItemWidth <$> fromAttribute a' <|> Failure "Not a valid Row Attirbute"

-- Label Attributes
data LabelAttributes = LabelWidth WidthAttr
                            deriving (Generic, Show)

instance ToJSON LabelAttributes where
instance FromJSON LabelAttributes where

instance AttributeClass LabelAttributes where
   toAttribute (LabelWidth a) = toAttribute a
   fromAttribute  = tryAllLabelAttributes
     where
       tryAllLabelAttributes a' = LabelWidth <$> fromAttribute a' <|> Failure "Not a valid Row Attirbute"

-- Input Attributes
data InputAttribute = InputWidth WidthAttr | InputType InputTypeAttr deriving (Generic, Show, Ord, Eq)

instance ToJSON InputAttribute where
instance FromJSON InputAttribute where

instance AttributeClass InputAttribute where
   toAttribute (InputWidth a) = toAttribute a
   toAttribute (InputType i) = toAttribute i
   fromAttribute = tryAllTypeAttributes
     where
       tryAllTypeAttributes a' = InputWidth <$> fromAttribute a' <|> InputType <$> fromAttribute a' <|> Failure "Not a valid input Attribute"

-- Button Atrributes
data ButtonAttributes = ButtonWidth WidthAttr | ButtonAction ActionAttr deriving (Generic, Show)

instance ToJSON ButtonAttributes where
instance FromJSON ButtonAttributes where

instance AttributeClass ButtonAttributes where
   toAttribute (ButtonWidth w) = toAttribute w
   toAttribute (ButtonAction a) = toAttribute a
   fromAttribute  = tryAllButtonAttributes
     where
       tryAllButtonAttributes a' = ButtonWidth <$> fromAttribute a'  <|>
                                   ButtonAction <$> fromAttribute a' <|>
                                   Failure "Not a valid button Attirbute"

-- Input Type
data Input = Input {
              _getInput    :: InputType,
              _inputAttrib :: [InputAttribute]

} deriving (Generic, Show, Ord, Eq, Typeable)

instance ToJSON Input where
instance FromJSON Input where

-- Input type canbe Text input or Signature input
data InputType = InputTypeText InputText
                |InputTypeSignature Signature
                |InputTypeInt InputInt
                |InputTypeDouble InputDouble deriving (Generic, Show, Ord, Eq, Typeable)

instance ToJSON InputType where
instance FromJSON InputType where

-- Text Type Input
newtype InputText = InputText { _getInputText::Text  } deriving (Generic, Show, Ord, Eq, Typeable)

instance ToJSON InputText where
instance FromJSON InputText where

-- Signature Type Input store as base64 encode Bytestring
newtype Signature = Signature {
_signature :: Text
 } deriving (Generic, Show, Ord, Eq, Typeable)

instance ToJSON Signature where
instance FromJSON Signature where

instance AttributeClass InputType where
  toAttribute (InputTypeText _) = Attribute "type" "'text'"
  toAttribute (InputTypeSignature _) = Attribute "type" "'signature'"
  toAttribute (InputTypeInt _) = Attribute "type" "int"
  toAttribute (InputTypeDouble _) = Attribute "type" "double"

-- Number Type Input
newtype InputInt = InputInt { _getInputInt::Int  } deriving (Generic, Show, Ord, Eq, Typeable)

instance ToJSON InputInt where
instance FromJSON InputInt where

-- Number Type Double
newtype InputDouble = InputDouble { _getInputDouble::Double } deriving (Generic, Show, Ord, Eq, Typeable)

instance ToJSON InputDouble where
instance FromJSON InputDouble where

-- | Utility Function
wrongAttrResponse :: Text -> Text -> Validation Text a
wrongAttrResponse correct other = Failure $ "Not " <> correct <> "attribute, recieved --> "  <> other
