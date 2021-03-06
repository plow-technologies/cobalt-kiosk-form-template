{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Kiosk.Backend.Form.Generator.Cobalt (insertThisFormInCobalt,
                                            updateThisFormInCobalt,
                                            currentCobaltForms) where

import           Kiosk.Backend.Form

import           Data.Aeson
import           Network.Wreq         (Response, post)

-- import Data.Traversable
import           Data.Monoid          ((<>))

import           Data.Text            (Text, pack)

import           Data.ByteString.Lazy (ByteString)
import           Data.String          (IsString)


insertThisFormInCobalt :: String -> String -> CobaltWaterHaulingCompany -> IO (Either Text (Response ByteString))
insertThisFormInCobalt url port whc@(CobaltWaterHaulingCompany Nothing _ _) = fmap Right $ post ("http://" <>
                                                                                                    url  <>
                                                                                                    ":"  <>
                                                                                                    port <>
                                                                                                    "/form/add") (encode  [convertToKioskForm $ whc])
insertThisFormInCobalt _url _port (CobaltWaterHaulingCompany (Just _) _ _)  = return $ Left "can't insert form that already has Id"


updateThisFormInCobalt :: String -> String -> CobaltWaterHaulingCompany -> IO (Either Text (Response ByteString))
updateThisFormInCobalt url port whc@(CobaltWaterHaulingCompany (Just i) _wc _u) = fmap Right $ post ("http://" <>
                                                                                               url <>
                                                                                               ":" <>
                                                                                               port <>
                                                                                               "/form/update?formid=" <>
                                                                                               (show i)) (encode.convertToKioskForm $ whc)
updateThisFormInCobalt _url _port whc@(CobaltWaterHaulingCompany Nothing _wc _u) = return $ Left "Can't update form w/o id"

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
                 , driverNameRow
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
    driverNameRow = generateInputRowText "Driver's Name"
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
                                                    , ItemAutoInput fullDefaultInputDate] []

fullDefaultInputDate :: Input
fullDefaultInputDate = Input fullDefaultInputTypeDate [InputType InputTypeAttributeDate]

fullDefaultInputTypeDate :: InputType
fullDefaultInputTypeDate = InputTypeDate $ (InputDate "")

-- Input Time
generateInputRowTime :: Text -> Row
generateInputRowTime labelTime = Row [generateInputItemTime labelTime] []

generateInputItemTime :: Text -> Item
generateInputItemTime  labelTime = Item [ItemLabel . generateLabel $ labelTime
                                                    , ItemAutoInput fullDefaultInputTime] []

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


convertToKioskForm :: CobaltWaterHaulingCompany -> Form
convertToKioskForm waterHaulingCompany = Form cobaltEnvironmentalSolutions cobaltAddress defaultLogo defaultPhone [createWaterHauler waterHaulingName] cobaltFormBody
  where
    waterHaulingName = _whcCompanyName $ waterHaulingCompany



data CobaltWaterHaulingCompany = CobaltWaterHaulingCompany { _whcFormId:: Maybe FormId
                                               , _whcCompanyName        :: CompanyName
                                               , _whcGetUUID            :: UUID }
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
                   | SoonerStar
                   | NexStream
                   | ZeroSeven
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
  show (SoonerStar    ) = "Sooner Star"
  show (NexStream    ) = "NexStream"
  show (ZeroSeven)     = "07 Energy"

currentCobaltForms :: [CobaltWaterHaulingCompany]
currentCobaltForms = [ CobaltWaterHaulingCompany (Just 0) BigStarTrucking exampleUUID
               , CobaltWaterHaulingCompany (Just 1) BulletEnergyServices exampleUUID
               , CobaltWaterHaulingCompany (Just 2) CandJTrucking exampleUUID
               , CobaltWaterHaulingCompany (Just 3) BigMacTankTrucks exampleUUID
               , CobaltWaterHaulingCompany (Just 4) BradyWeldingandMachineShop exampleUUID
               , CobaltWaterHaulingCompany (Just 5) KleenOilfieldServices exampleUUID
               , CobaltWaterHaulingCompany (Just 6) BandCBackhoeandTransports exampleUUID
               , CobaltWaterHaulingCompany (Just 7) ForsytheOilfield exampleUUID
               , CobaltWaterHaulingCompany (Just 8) HullsOilfield exampleUUID
               , CobaltWaterHaulingCompany (Just 9) SouthCentralOilfieldServices exampleUUID
               , CobaltWaterHaulingCompany (Just 10) TopOTexas exampleUUID
               , CobaltWaterHaulingCompany (Just 11) MitchellTankTruckServices exampleUUID
               , CobaltWaterHaulingCompany (Just 12) FluidServices exampleUUID
               , CobaltWaterHaulingCompany (Just 13) DavenportOilfieldServices exampleUUID
               , CobaltWaterHaulingCompany (Just 14) TestCompany exampleUUID
               , CobaltWaterHaulingCompany (Just 15) SoonerStar exampleUUID
               , CobaltWaterHaulingCompany (Just 16) NexStream exampleUUID
               , CobaltWaterHaulingCompany  (Just 19)  ZeroSeven exampleUUID                                           ]


-- | Use this to pretend everything is new
currentCobaltFormsNothingMode (CobaltWaterHaulingCompany _ c e) = CobaltWaterHaulingCompany Nothing c e

postToUserAndIdInsert uuid username formId = post "http://alarm.plowtech.net:4600/user/key/join/insert" (toJSON (uuid,username,formId) )
postToUserAndIdDelete uuid username  = post "http://alarm.plowtech.net:4600/user/key/join/delete" (toJSON (uuid,username) )


useridlist = [ "bigstar"
             , "bullet"
             , "cjtrucking"
             , "bigmac"
             , "bradywelding"
             , "kleen"
             , "bcbackhoe"
             , "forsythe"
             , "hulls"
             , "southcentraloil"
             , "topotexas"
             , "mitchelltank"
             , "fluidservices"
             , "davenport"
             , "testcorp"
             , "soonerstar"
             , "nexstream"


               ]

formIdList = [0 .. (length useridlist)]
