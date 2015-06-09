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
import           Data.Aeson                            (FromJSON, ToJSON)
import qualified Data.Text                             as T
import           GHC.Generics                          (Generic)
import           Kiosk.Backend.Form.Attribute          (Attribute (..),
                                                        AttributeClass (..))
import           Kiosk.Backend.Form.Element.Item.Input
import           Kiosk.Backend.Form.Element.Item.Label (Label (..),
                                                        defaultLabel)
data Option = Option {
    _getOptionText :: T.Text
  , _optionAttrib  :: [OptionAttributes]
} deriving (Generic, Show)


instance ToJSON Option where
instance FromJSON Option where

data OptionAttributes = OptionNull deriving (Show,Generic)


instance ToJSON OptionAttributes where
instance FromJSON OptionAttributes where

instance AttributeClass OptionAttributes where
        toAttribute (OptionNull) = Attribute "" ""
        fromAttribute _ = Right OptionNull


data OptionQualifier = OptionQualifier {
    _getOptionQualifierText :: [QualifierChoices]
  , _optionQualifierAttrib  :: [OptionQualifierAttributes]
} deriving (Generic, Show )


instance ToJSON OptionQualifier where
instance FromJSON OptionQualifier where

data OptionQualifierAttributes = OptionQualifierNull deriving (Show,Generic)


instance ToJSON OptionQualifierAttributes where
instance FromJSON OptionQualifierAttributes where

instance AttributeClass OptionQualifierAttributes where
        toAttribute (OptionQualifierNull) = Attribute "" ""
        fromAttribute _ = Right OptionQualifierNull

data QualifierChoices = QualifierInput Input | QualifierLabel Label deriving (Show,Generic)


instance ToJSON QualifierChoices where
instance FromJSON QualifierChoices where
