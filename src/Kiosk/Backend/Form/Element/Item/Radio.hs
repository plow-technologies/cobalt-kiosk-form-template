{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}


{- |
Module      :  Kiosk.Backend.Form.Element.Item.Radio
Description :  Radio Element, this is where json is created
Copyright   :  Plow Technologies LLC
License     :  MIT License

Maintainer  :  Scott Murphy
Stability   :  experimental
Portability :  portable



-}


module Kiosk.Backend.Form.Element.Item.Radio  ( Radio(..)
                                              , Option(..)
                                              , OptionQualifier(..)
                                              , defaultRadio) where


import Data.Aeson (ToJSON
                  ,FromJSON)
import GHC.Generics (Generic)
import Data.Text (Text)
import Kiosk.Backend.Form.Element.Item.Label (Label(..))
import Kiosk.Backend.Form.Element.Item.Input 
import Kiosk.Backend.Form.Attribute (AttributeClass(..)
                                    ,Attribute(..) )
import           Data.Either.Validation (Validation(..))


import Kiosk.Backend.Form.Attribute.Width 
import Kiosk.Backend.Form.Attribute.Indexable

-- A Radio button is a circular, singular selector
-- Our radio buttons come preloaded with Options!


-- |Radio Button parent element
data Radio = Radio { _getRadioOptions :: [Option]
                   ,  _getRadioQualifier :: [OptionQualifier]
            } deriving (Generic, Show)


instance ToJSON Radio where
instance FromJSON Radio where 
-- | Option for radio button 

data Option = Option { _getOptionText :: Text
                     , _optionAttrib :: [OptionAttributes]                   
             } deriving (Generic, Show)

instance ToJSON Option where 
instance FromJSON Option where         

data OptionAttributes = OptionNull
  deriving (Generic, Show)
 
instance ToJSON OptionAttributes where
instance FromJSON OptionAttributes where  


instance AttributeClass OptionAttributes where 
         toAttribute (OptionNull) = Attribute "" ""
         fromAttribute _ = Success OptionNull                                            


         
-- --------------------------------------------------         

   
-- | Option Qualifier (like a text field that is paired with the output   
-- It is a separate element and at the same level as Option
   
data OptionQualifier = OptionQualifier { _getOptionQualifierText :: [QualifierChoices]
                                       , _optionQualifierAttrib :: [OptionQualifierAttributes]                   
                      } deriving (Generic, Show)

instance ToJSON OptionQualifier where 
instance FromJSON OptionQualifier where         

data OptionQualifierAttributes = OptionQualifierNull
  deriving (Generic, Show)

  
instance ToJSON OptionQualifierAttributes where  
instance FromJSON OptionQualifierAttributes where

instance AttributeClass OptionQualifierAttributes where 
         toAttribute (OptionQualifierNull) = Attribute "" ""
         fromAttribute _ = Success OptionQualifierNull                  


data QualifierChoices = QualifierInput Input | QualifierLabel Label
  deriving (Show,Generic)                                                              


instance ToJSON QualifierChoices where 
instance FromJSON QualifierChoices where 

-- | DEFAULTS --------------------------

defaultRadio :: Radio
defaultRadio = Radio [defaultOption] [defaultOptionQualifier]

defaultOption :: Option 
defaultOption = Option "Pit Water" []

defaultOptionQualifier :: OptionQualifier
defaultOptionQualifier = OptionQualifier defaultQualifierChoices []

defaultQualifierChoices :: [QualifierChoices]
defaultQualifierChoices = [QualifierLabel ( Label "Amount" [])
                          ,QualifierInput defaultQualifierInput]

defaultQualifierInput :: Input
defaultQualifierInput = Input dit dia
 where 
   dit = InputTypeText . InputText $ "" 
   dia = [wAttr, tAttr, ixAttr]
   wAttr = InputWidth $ WidthAttribute (12::Int)
   ixAttr = InputIndexable $ IndexableAttribute True
   tAttr = InputType $ InputTypeAttribute dit


