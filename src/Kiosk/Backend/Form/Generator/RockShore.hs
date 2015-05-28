{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}  
module Kiosk.Backend.Form.Generator.RockShore ( currentRockshoreForms) where
                                              
import Kiosk.Backend.Form
import qualified Data.Text as T
import Data.String (IsString)

rockShoreEnergy :: Company
rockShoreEnergy  = Company "Rock Shore Energy, LLC" [CompanyWidth $ WidthAttribute (12::Int) ]

rockShoreAddress:: Address
rockShoreAddress= Address "3824 CedarSprings Road, Suite 450\nDallas Texas 75219" [AddressWidth $ WidthAttribute (12::Int)]



createWaterHauler :: CompanyName  -> Constant
createWaterHauler whc = Constant (T.pack . show $ whc)  [ ConstantAttributeType "'Water Hauling Company'"
                                                                , ConstantAttributeIndexable $ IndexableAttribute True ]                   
rockShoreFormBody :: [Row]
rockShoreFormBody = [ truckNumberRow
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
    leaseInfoRow  = generateLabelRow "Lease or Well Information"
    leaseOperatorRow = generateInputRowText "Name of Lease/Well Operator"
    leaseNameRow = generateInputRowText "Name of Lease or Well"
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

generateLabelRow :: T.Text -> Row
generateLabelRow labelText = Row [generateLabelItem labelText] []                   

generateLabelItem :: T.Text -> Item
generateLabelItem labelText = Item [ItemLabel . generateLabel $ labelText ] []


-- Input Text
generateInputRowText :: T.Text -> Row
generateInputRowText labelText = Row [generateInputItemText labelText] []

generateInputItemText :: T.Text -> Item
generateInputItemText  labelText = Item [ItemLabel . generateLabel $ labelText
                                                    , ItemInput fullDefaultInputText] []

fullDefaultInputText :: Input
fullDefaultInputText = Input fullDefaultInputTypeText fullDefaultInputAttributesList

fullDefaultInputTypeText :: InputType
fullDefaultInputTypeText = InputTypeText $ InputText ""

-- Input Date
generateInputRowDate :: T.Text -> Row
generateInputRowDate labelDate = Row [generateInputItemDate labelDate] []

generateInputItemDate :: T.Text -> Item
generateInputItemDate  labelDate = Item [ItemLabel . generateLabel $ labelDate
                                                    , ItemAutoInput . AutoInput $ fullDefaultInputDate] []

fullDefaultInputDate :: Input
fullDefaultInputDate = Input fullDefaultInputTypeDate [InputType InputTypeAttributeDate]

fullDefaultInputTypeDate :: InputType
fullDefaultInputTypeDate = InputTypeDate  (InputDate "")

-- Input Time
generateInputRowTime :: T.Text -> Row
generateInputRowTime labelTime = Row [generateInputItemTime labelTime] []

generateInputItemTime :: T.Text -> Item
generateInputItemTime  labelTime = Item [ItemLabel . generateLabel $ labelTime
                                                    , ItemAutoInput . AutoInput $ fullDefaultInputTime] []

fullDefaultInputTime :: Input
fullDefaultInputTime = Input fullDefaultInputTypeTime [InputType InputTypeAttributeTime]

fullDefaultInputTypeTime :: InputType
fullDefaultInputTypeTime = InputTypeTime  (InputTime "")

-- Input Signature

generateInputRowSignature :: T.Text -> Row
generateInputRowSignature labelText = Row [generateInputItemSignature labelText] []

generateInputItemSignature :: T.Text -> Item
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
                tAttr = InputType InputTypeAttributeText
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
   tAttr = InputType InputTypeAttributeDouble

generateLabel :: T.Text -> Label
generateLabel labelText = Label labelText [LabelWidth $ WidthAttribute (12::Int)]

generateRadio :: T.Text -> [Option] -> Radio
generateRadio labelText options = Radio (generateLabel labelText) options [fullDefaultOptionQualifier]

generateOption :: T.Text -> Option 
generateOption optionText = Option optionText []

convertToKioskForm :: RockShoreWaterHaulingCompany -> Form
convertToKioskForm waterHaulingCompany = Form rockShoreEnergy rockShoreAddress rockShoreLogo defaultPhone [createWaterHauler waterHaulingName] rockShoreFormBody
  where 
    waterHaulingName = _whcCompanyName waterHaulingCompany



data RockShoreWaterHaulingCompany = RockShoreWaterHaulingCompany { 
    _whcFormId:: Maybe FormId
  , _whcCompanyName :: CompanyName                                                          
  , _whcGetUUID :: UUID 
} deriving (Eq,Ord)                 

newtype FormId = FormId {
  _getFormId :: Integer
} deriving (Read,Eq,Show,Num,Ord)

newtype UUID = UUID { 
  _getUUID :: T.Text
} deriving (Read,Eq,Show,IsString,Ord)

exampleUUID :: UUID
exampleUUID = "ed0c7e53-f510-45c5-a024-5d6b1247c731"

data CompanyName = BigStarTrucking
                     | BulletEnergyServices
                     | Advantage
                     | ArkomaTanks
                     | BasicEnergyServices
                     | CottonwoodDrilling
                     | DalesTankService
                     | FluidServices
                     | GandCConstruction
                     | HammandPhillipsService
                     | HullsOilfieldService
                     | JNSTrucking
                     | KleenOilfieldService
                     | LaurcoEnergies 
                     | MSMEnvironmental
                     | Nabors
                     | RHRServices
                     | SandHTankService
                     | SandM
                     | TestCompany
                     | AandATankTruck
                     | SonnyTrucking
                     | TerracoProductionLLC
                     | BigMacTrucking        

          deriving (Eq,Ord)

instance Show CompanyName where
  show (BigStarTrucking) = "Big Star Trucking"
  show (BulletEnergyServices) = "Bullet Energy Services"
  show (Advantage) = "Advantage"
  show (ArkomaTanks) = "Arkoma Tanks LLC"
  show (BasicEnergyServices) = "Basic Energy Services"
  show (CottonwoodDrilling) = "Cottonwood Drilling"
  show (DalesTankService) = "Dales Tank Service"
  show (FluidServices) = "Fluid Services LLC"
  show (GandCConstruction) = "G and C Construction International"
  show (HammandPhillipsService) = "Hamm and Phillips Service Co"
  show (HullsOilfieldService) = "Hulls Oilfield Trucking"
  show (JNSTrucking) = "JNS Trucking"
  show (KleenOilfieldService) = "Kleen Oilfield Service Co"
  show (LaurcoEnergies) = "Laurco Energies Inc"
  show (MSMEnvironmental) = "MSM Environmental LLC"
  show (Nabors) = "Nabors"
  show (RHRServices) = "RHR Services"
  show (SandHTankService) = "S and H Tank Service"
  show (SandM) = "S and M"
  show (TestCompany) = "Test Company"
  show (AandATankTruck) = "A and A Tank Truck Co"
  show (SonnyTrucking) = "Sonny Trucking "
  show (TerracoProductionLLC) = "Terraco Production LLC"  
  show (BigMacTrucking) = "Big Mac Trucking"

currentRockshoreForms :: [RockShoreWaterHaulingCompany]
currentRockshoreForms = [ RockShoreWaterHaulingCompany (Just 0)  BigStarTrucking exampleUUID
                        , RockShoreWaterHaulingCompany (Just 1)  BulletEnergyServices exampleUUID
                        , RockShoreWaterHaulingCompany (Just 2)  Advantage exampleUUID
                        , RockShoreWaterHaulingCompany (Just 3)  ArkomaTanks exampleUUID
                        , RockShoreWaterHaulingCompany (Just 4)  BasicEnergyServices exampleUUID
                        , RockShoreWaterHaulingCompany (Just 5)  CottonwoodDrilling exampleUUID
                        , RockShoreWaterHaulingCompany (Just 6)  DalesTankService exampleUUID
                        , RockShoreWaterHaulingCompany (Just 7)  FluidServices exampleUUID
                        , RockShoreWaterHaulingCompany (Just 8)  GandCConstruction exampleUUID
                        , RockShoreWaterHaulingCompany (Just 9)  HammandPhillipsService exampleUUID
                        , RockShoreWaterHaulingCompany (Just 10)    HullsOilfieldService exampleUUID
                        , RockShoreWaterHaulingCompany (Just 11)    JNSTrucking exampleUUID
                        , RockShoreWaterHaulingCompany (Just 12)    KleenOilfieldService exampleUUID
                        , RockShoreWaterHaulingCompany (Just 13)    LaurcoEnergies exampleUUID
                        , RockShoreWaterHaulingCompany (Just 14)    MSMEnvironmental exampleUUID
                        , RockShoreWaterHaulingCompany (Just 15)    Nabors exampleUUID
                        , RockShoreWaterHaulingCompany (Just 16)    RHRServices exampleUUID
                        , RockShoreWaterHaulingCompany (Just 17)    SandHTankService exampleUUID
                        , RockShoreWaterHaulingCompany (Just 18)    SandM exampleUUID
                        , RockShoreWaterHaulingCompany (Just 19)    TestCompany exampleUUID
                        , RockShoreWaterHaulingCompany (Just 20)    AandATankTruck exampleUUID
                        , RockShoreWaterHaulingCompany (Just 21)    SonnyTrucking exampleUUID               
                        , RockShoreWaterHaulingCompany (Just 22)    TerracoProductionLLC exampleUUID
                        , RockShoreWaterHaulingCompany (Just 23)    BigMacTrucking exampleUUID]

  
  -- | Use this to pretend everything is new                        
currentRockshoreFormsNothingMode (RockShoreWaterHaulingCompany _ c e) = RockShoreWaterHaulingCompany Nothing c e

rockShoreLogo :: Logo
rockShoreLogo = Logo "" [LogoPath . PathAttribute $ "'RockShoreEnergy.png'"  ]
