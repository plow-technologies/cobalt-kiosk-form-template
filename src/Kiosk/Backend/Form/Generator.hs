
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}  

{- |
Module      :  Kiosk.Backend.Form.Generator
Description :  Generate Forms for current customers
Copyright   :  Plow Technologies LLC 
License     :  MIT License

Maintainer  :  Scott Murphy 
Stability   :  experimental
Portability :  portable

Generate various forms for our customers.
         
Try and keep as much generation data in our DB as possible.

         

-}

module Kiosk.Backend.Form.Generator ( updateThisThing) where 

import Kiosk.Backend.Form

import Network.Wreq (Response
                    ,post)
import Data.Aeson

-- import Data.Traversable
import Data.Monoid ((<>))   

import Data.Text (pack
                 ,Text)
                 
import Data.String (IsString)
import Data.ByteString.Lazy (ByteString)

updateThisThing :: String -> WaterHaulingCompany -> IO (Response ByteString)
updateThisThing url whc@(WaterHaulingCompany i _wc _u) = post ("http://" <> 
                                                               url <>
                                                               ":2833/form/update?formid=" <>
                                                               (show i)) (encode.cobaltKioskForm $ whc)


cobaltEnvironmentalSolutions :: Company
cobaltEnvironmentalSolutions  = Company "Cobalt Environmental Solutions LLC" [CompanyWidth $ WidthAttribute (12::Int) ]

cobaltAddress:: Address
cobaltAddress= Address "PO Box 130 Wilson, Oklahoma 73463\n886-849-5483\n" [AddressWidth $ WidthAttribute (12::Int)]



createWaterHauler :: CompanyName  -> Constant
createWaterHauler whc = Constant (pack.show $ whc)  [ ConstantAttributeType "'Water Hauling Company'"
                                                                , ConstantAttributeIndexable $ IndexableAttribute True ]                   
cobaltFormBody :: [Row]
cobaltFormBody = [ truckNumberRow
                 , permitNumberRow
                 , customerTicketNumberRow
                 , leaseInfoRow
                 , leaseOperatorRow
                 , leaseNameRow 
                 , waterTypeAndAmountRow
                 , dateRow
                 , timeInRow
                 , signatureRow]
  where 
    truckNumberRow  = generateInputRowText "Truck #"
    permitNumberRow  = generateInputRowText "Water Hauling Permit #"
    customerTicketNumberRow = generateInputRowText "Customer Ticket #"
    leaseInfoRow  = generateLabelRow "Lease Information"
    leaseOperatorRow = generateInputRowText "Name of Lease Operator"
    leaseNameRow = generateInputRowText "Name of Lease"
    waterTypeAndAmountRow  = waterTypeRadioRow
    dateRow  = generateInputRowDate "Date"
    timeInRow  = generateInputRowTime "Time In"
    signatureRow  = generateInputRowSignature "Driver Signature"









waterTypeRadioRow :: Row 
waterTypeRadioRow = Row [waterTypeRadio] []            


waterTypeRadio :: Item
waterTypeRadio  = Item [ItemRadio . generateRadio "Type of Water Hauled" $ options ] []                     
   where
     options = [generateOption "Produced Water"
               ,generateOption "Pit Water"
               ,generateOption "Fresh Water"
               ,generateOption "Flowback Water" ]

generateLabelRow :: Text -> Row
generateLabelRow labelText = Row [generateLabelItem labelText] []                   

generateLabelItem :: Text -> Item
generateLabelItem labelText = Item [ItemLabel . generateLabel $ labelText ] []


-- Input Text
generateInputRowText :: Text -> Row
generateInputRowText labelText = Row [generateInputItemText labelText] []

generateInputItemText :: Text -> Item
generateInputItemText  labelText = Item [ItemLabel . generateLabel $ labelText
                                                    , ItemInput fullDefaultInputText] []

fullDefaultInputText :: Input
fullDefaultInputText = Input fullDefaultInputTypeText fullDefaultInputAttributesList

fullDefaultInputTypeText :: InputType
fullDefaultInputTypeText = InputTypeText $ InputText (""::Text)

-- Input Date
generateInputRowDate :: Text -> Row
generateInputRowDate labelDate = Row [generateInputItemDate labelDate] []

generateInputItemDate :: Text -> Item
generateInputItemDate  labelDate = Item [ItemLabel . generateLabel $ labelDate
                                                    , ItemInput fullDefaultInputDate] []

fullDefaultInputDate :: Input
fullDefaultInputDate = Input fullDefaultInputTypeDate [InputType InputTypeAttributeDate]

fullDefaultInputTypeDate :: InputType
fullDefaultInputTypeDate = InputTypeDate $ (InputDate "")

-- Input Time
generateInputRowTime :: Text -> Row
generateInputRowTime labelTime = Row [generateInputItemTime labelTime] []

generateInputItemTime :: Text -> Item
generateInputItemTime  labelTime = Item [ItemLabel . generateLabel $ labelTime
                                                    , ItemInput fullDefaultInputTime] []

fullDefaultInputTime :: Input
fullDefaultInputTime = Input fullDefaultInputTypeTime [InputType InputTypeAttributeTime]

fullDefaultInputTypeTime :: InputType
fullDefaultInputTypeTime = InputTypeTime $ (InputTime "")

-- Input Signature

generateInputRowSignature :: Text -> Row
generateInputRowSignature labelText = Row [generateInputItemSignature labelText] []

generateInputItemSignature :: Text -> Item
generateInputItemSignature  labelText = Item [ItemLabel . generateLabel $ labelText
                                                    , ItemInput fullDefaultInputSignature] []

fullDefaultInputSignature :: Input
fullDefaultInputSignature = Input fullDefaultInputTypeSignature [InputType InputTypeAttributeSignature]

fullDefaultInputTypeSignature :: InputType
fullDefaultInputTypeSignature = InputTypeSignature $ Signature ""

fullDefaultInputAttributesList :: [InputAttribute]
fullDefaultInputAttributesList = [tAttr, ixAttr]
              where 
                ixAttr = InputIndexable $ IndexableAttribute True
                tAttr = InputType $ InputTypeAttributeText
-- | Radio


fullDefaultOptionQualifier :: OptionQualifier
fullDefaultOptionQualifier = OptionQualifier fullDefaultQualifierChoices []

fullDefaultQualifierChoices :: [QualifierChoices]
fullDefaultQualifierChoices = [ QualifierLabel ( Label "Amount" [])
                              , QualifierInput fullDefaultQualifierInput]

fullDefaultQualifierInput :: Input
fullDefaultQualifierInput = Input dit dia
 where 
   dit = InputTypeDouble . InputDouble $ 0.0
   dia = [tAttr, ixAttr,minAttr,maxAttr]
   minAttr = InputMinDouble $ MinAttributeDouble (0.0::Double)
   maxAttr = InputMaxDouble $ MaxAttributeDouble (150.0::Double)   
   ixAttr = InputIndexable $ IndexableAttribute True
   tAttr = InputType $ InputTypeAttributeDouble

generateLabel :: Text -> Label
generateLabel labelText = Label labelText [LabelWidth $ WidthAttribute (12::Int)]

generateRadio :: Text -> [Option] -> Radio
generateRadio labelText options = Radio (generateLabel labelText) options [fullDefaultOptionQualifier]

generateOption :: Text -> Option 
generateOption optionText = Option optionText []


cobaltKioskForm :: WaterHaulingCompany -> Form
cobaltKioskForm waterHaulingCompany = Form cobaltEnvironmentalSolutions cobaltAddress defaultLogo defaultPhone [createWaterHauler waterHaulingName] cobaltFormBody
  where 
    waterHaulingName = _whcCompanyName $ waterHaulingCompany

data WaterHaulingCompany = WaterHaulingCompany { _whcFormId::FormId
                                               , _whcCompanyName :: CompanyName                                                          
                                               , _whcGetUUID :: UUID }
                                deriving (Eq,Ord)                 

newtype FormId = FormId {_getFormId :: Integer}
            deriving (Read,Eq,Show,Num,ToJSON,FromJSON,Ord)

newtype UUID = UUID { _getUUID :: Text}
            deriving (Read,Eq,Show,IsString,ToJSON,FromJSON,Ord)

exampleUUID :: UUID
exampleUUID = "a2e3609e-154d-4e60-80e0-c77189098617"



data CompanyName = BigStarTrucking
                   | BulletEnergyServices
                   | CandJTrucking
                   | BigMacTankTrucks
                   | BradyWeldingandMachineShop
                   | KleenOilfieldServices
                   | BandCBackhoeandTransports
                   | ForsytheOilfield 
                   | HullsOilfield
                   | SouthCentralOilfieldServices
                   | TopOTexas
                   | MitchellTankTruckServices
                   | FluidServices
                   | DavenportOilfieldServices
                   | TestCompany
          deriving (Eq,Ord)


instance Show CompanyName where
  show (BigStarTrucking) = "Big Star Trucking"
  show (BulletEnergyServices) = "Bullet Energy Services"
  show (CandJTrucking) = "C and J Trucking"
  show (BigMacTankTrucks) = "Big Mac Trucks"
  show (BradyWeldingandMachineShop) = "Bradly Welding and Machine Shop"
  show (KleenOilfieldServices) = "Kleen Oilfield Services"
  show (BandCBackhoeandTransports) = "B and C Backhoe and Transports"
  show (ForsytheOilfield ) = "Forsythe Oilfield"
  show (HullsOilfield) = "Hulls Oilfield"
  show (SouthCentralOilfieldServices) = "South Central Oilfield Services"
  show (TopOTexas) = "Top-O-Texas"
  show (MitchellTankTruckServices) = "Mitchell Tank Truck Services"
  show (FluidServices) = "Fluid Services"
  show (DavenportOilfieldServices) = "Davenport Oilfield Services"
  show (TestCompany    ) = "Test Company"


currentForms :: [WaterHaulingCompany]
currentForms = [ WaterHaulingCompany 0 BigStarTrucking exampleUUID
               , WaterHaulingCompany 1 BulletEnergyServices exampleUUID
               , WaterHaulingCompany 2 CandJTrucking exampleUUID
               , WaterHaulingCompany 3 BigMacTankTrucks exampleUUID
               , WaterHaulingCompany 4 BradyWeldingandMachineShop exampleUUID
               , WaterHaulingCompany 5 KleenOilfieldServices exampleUUID
               , WaterHaulingCompany 6 BandCBackhoeandTransports exampleUUID
               , WaterHaulingCompany 7 ForsytheOilfield exampleUUID
               , WaterHaulingCompany 8 HullsOilfield exampleUUID
               , WaterHaulingCompany 9 SouthCentralOilfieldServices exampleUUID
               , WaterHaulingCompany 10 TopOTexas exampleUUID
               , WaterHaulingCompany 11 MitchellTankTruckServices exampleUUID
               , WaterHaulingCompany 12 FluidServices exampleUUID
               , WaterHaulingCompany 13 DavenportOilfieldServices exampleUUID
               , WaterHaulingCompany 14 TestCompany exampleUUID]
