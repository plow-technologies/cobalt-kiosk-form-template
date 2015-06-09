
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}


module Kiosk.Backend.Form.Element.Item.Radio  ( Radio(..)
                                              , Option(..)
                                              , OptionAttributes(..)
                                              , OptionQualifier(..)
                                              , OptionQualifierAttributes(..)
                                              , QualifierChoices (..)
                                              , defaultRadio) where

import qualified Data.Text                              as T
import           GHC.Generics                           (Generic)
import           Kiosk.Backend.Form.Attribute           (Attribute (..),
                                                         AttributeClass (..))
import           Kiosk.Backend.Form.Element.Item.Input
import           Kiosk.Backend.Form.Element.Item.Label  (Label (..),
                                                         defaultLabel)

import           Data.Aeson                             (FromJSON, ToJSON)
import           Kiosk.Backend.Form.Attribute.Indexable
import           Kiosk.Backend.Form.Attribute.Width

-- A Radio button is a circular, singular selector
-- Our radio buttons come preloaded with Options!


-- |Radio Button parent element
data Radio = Radio {
    _getRadioLabel     :: Label
  , _getRadioOptions   :: [Option]
  , _getRadioQualifier :: [OptionQualifier]
} deriving (Generic, Show)


instance ToJSON Radio where
instance FromJSON Radio where

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

defaultRadio :: Radio
defaultRadio = Radio defaultLabel [defaultOption] [defaultOptionQualifier]

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
   dia = [wAttr, tAttr, ixAttr]
   wAttr = InputWidth $ WidthAttribute (12::Int)
   ixAttr = InputIndexable $ IndexableAttribute True
   tAttr = InputType $ InputTypeAttributeText
