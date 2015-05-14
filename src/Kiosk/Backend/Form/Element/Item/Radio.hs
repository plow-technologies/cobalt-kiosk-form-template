{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}


module Kiosk.Backend.Form.Element.Item.Radio  ( Radio(..)
                                              , Option(..)
                                              , OptionQualifier(..)
                                              , QualifierChoices (..)
                                              , defaultRadio) where

import GHC.Generics (Generic)
import qualified Data.Text as T
import Kiosk.Backend.Form.Element.Item.Label ( Label(..)
                                             , defaultLabel)
import Kiosk.Backend.Form.Element.Item.Input 
import Kiosk.Backend.Form.Attribute ( AttributeClass(..)
                                    , Attribute(..) )

import Kiosk.Backend.Form.Attribute.Width 
import Kiosk.Backend.Form.Attribute.Indexable

-- A Radio button is a circular, singular selector
-- Our radio buttons come preloaded with Options!


-- |Radio Button parent element
data Radio = Radio { 
    _getRadioLabel :: Label
  , _getRadioOptions :: [Option]
  , _getRadioQualifier :: [OptionQualifier]
} deriving (Generic, Show)                                 

data Option = Option { 
    _getOptionText :: T.Text
  , _optionAttrib :: [OptionAttributes]                   
} deriving (Generic, Show)

data OptionAttributes = OptionNull deriving (Show)


instance AttributeClass OptionAttributes where 
	toAttribute (OptionNull) = Attribute "" ""
	fromAttribute _ = Right OptionNull   


data OptionQualifier = OptionQualifier { 
    _getOptionQualifierText :: [QualifierChoices]
  , _optionQualifierAttrib :: [OptionQualifierAttributes]                   
} deriving (Generic, Show)

data OptionQualifierAttributes = OptionQualifierNull deriving (Show)

instance AttributeClass OptionQualifierAttributes where 
	toAttribute (OptionQualifierNull) = Attribute "" ""
	fromAttribute _ = Right OptionQualifierNull                  

data QualifierChoices = QualifierInput Input | QualifierLabel Label deriving (Show)


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