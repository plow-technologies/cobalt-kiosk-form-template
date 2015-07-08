{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}


{- |
Module      :  Kiosk.Backend.Form.Element.Item.Option
Description :  Options for inputs radios and other items
Copyright   :  Plow Technologies LLC
License     :  MIT License

Maintainer  :  Scott Murphy
Stability   :  experimental
Portability :  portable

<radio>
 <option>

-}


module  Kiosk.Backend.Form.Element.Item.Option  where
import           Data.Aeson                             (FromJSON, ToJSON)
import qualified Data.Text                              as T
import           Data.Typeable                          (Typeable)

import           GHC.Generics                           (Generic)
import           Kiosk.Backend.Form.Attribute           (Attribute (..),
                                                         AttributeClass (..))
import           Kiosk.Backend.Form.Attribute.Indexable
import           Kiosk.Backend.Form.Attribute.Required
import           Kiosk.Backend.Form.Attribute.Width
import           Kiosk.Backend.Form.Element.Item.Input
import           Kiosk.Backend.Form.Element.Item.Label  (Label (..))

data Option = Option {
    _getOptionText :: T.Text
  , _optionAttrib  :: [OptionAttributes]
} deriving (Generic, Show, Typeable)


instance ToJSON Option where
instance FromJSON Option where

data OptionAttributes = OptionNull deriving (Generic, Show, Ord, Eq, Typeable)


instance ToJSON OptionAttributes where
instance FromJSON OptionAttributes where

instance AttributeClass OptionAttributes where
        toAttribute (OptionNull) = Attribute "" ""
        fromAttribute _ = Right OptionNull


data OptionQualifier = OptionQualifier {
    _getOptionQualifierText :: [QualifierChoices]
  , _optionQualifierAttrib  :: [OptionQualifierAttributes]
} deriving (Generic, Show, Typeable)


instance ToJSON OptionQualifier where
instance FromJSON OptionQualifier where

data OptionQualifierAttributes = OptionQualifierNull deriving (Generic, Show, Ord, Eq, Typeable)


instance ToJSON OptionQualifierAttributes where
instance FromJSON OptionQualifierAttributes where

instance AttributeClass OptionQualifierAttributes where
        toAttribute (OptionQualifierNull) = Attribute "" ""
        fromAttribute _ = Right OptionQualifierNull

data QualifierChoices = QualifierInput Input | QualifierLabel Label deriving (Generic, Show, Typeable)


instance ToJSON QualifierChoices where
instance FromJSON QualifierChoices where


defaultOption :: Option
defaultOption = Option "Pit Water" []

defaultOptionQualifier :: OptionQualifier
defaultOptionQualifier = OptionQualifier defaultQualifierChoices []

defaultQualifierChoices :: [QualifierChoices]
defaultQualifierChoices = [ QualifierLabel ( Label "Amount" [])
                          , QualifierInput defaultQualifierInput]

defaultQualifierInput :: Input
defaultQualifierInput = Input dit dia
 where
   dit = InputTypeText . InputText $ ""
   dia = [wAttr, tAttr, ixAttr, rAttr]
   wAttr = InputWidth $ WidthAttribute (12::Int)
   ixAttr = InputIndexable $ IndexableAttribute True
   rAttr  = InputRequired $ RequiredAttribute True
   tAttr = InputType $ InputTypeAttributeText
