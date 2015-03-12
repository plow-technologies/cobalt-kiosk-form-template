{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}  
module Kiosk.Backend.Form.Generator.RockShore ( insertThisFormInRockShore
                                              , updateThisFormInRockShore) where
                                              
import Kiosk.Backend.Form
import Network.Wreq (Response
                    ,post)
import Data.Aeson
import Data.Monoid ((<>))   
import Data.Text (pack
                 ,Text)
import Data.String (IsString)
import Data.ByteString.Lazy (ByteString)



insertThisFormInRockShore :: String -> String -> RockShoreWaterHaulingCompany -> IO (Either Text (Response ByteString))
insertThisFormInRockShore url port whc@(RockShoreWaterHaulingCompany Nothing _ _) = fmap Right $ post ("http://" <>
                                                                                                    url  <>
                                                                                                    ":"  <>
                                                                                                    port <>
                                                                                                    "/form/add") (encode  [convertToKioskForm $ whc])
insertThisFormInRockShore _url _port (RockShoreWaterHaulingCompany (Just _) _ _)  = return $ Left "can't insert form that already has Id"                                                                            

updateThisFormInRockShore :: String -> RockShoreWaterHaulingCompany -> IO (Either Text (Response ByteString))
updateThisFormInRockShore url whc@(RockShoreWaterHaulingCompany (Just i) _wc _u) = fmap Right $ post ("http://" <> 
                                                                                               url <>
                                                                                               ":2833/form/update?formid=" <>
                                                                                               (show i)) (encode.convertToKioskForm $ whc)                                                                                       
updateThisFormInRockShore _url whc@(RockShoreWaterHaulingCompany Nothing _wc _u) = return $ Left "Can't update form w/o id"

rockShoreEnergy :: Company
rockShoreEnergy  = Company "Rock Shore Energy, LLC" [CompanyWidth $ WidthAttribute (12::Int) ]

rockShoreAddress:: Address
rockShoreAddress= Address "3824 CedarSprings Road, Suite 450\nDallas Texas 75219" [AddressWidth $ WidthAttribute (12::Int)]



createWaterHauler :: CompanyName  -> Constant
createWaterHauler whc = Constant (pack.show $ whc)  [ ConstantAttributeType "'Water Hauling Company'"
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

convertToKioskForm :: RockShoreWaterHaulingCompany -> Form
convertToKioskForm waterHaulingCompany = Form rockShoreEnergy rockShoreAddress rockShoreLogo defaultPhone [createWaterHauler waterHaulingName] rockShoreFormBody
  where 
    waterHaulingName = _whcCompanyName $ waterHaulingCompany



data RockShoreWaterHaulingCompany = RockShoreWaterHaulingCompany { _whcFormId:: Maybe FormId
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


currentForms :: [RockShoreWaterHaulingCompany]
currentForms = [ RockShoreWaterHaulingCompany Nothing BigStarTrucking ed0c7e53-f510-45c5-a024-5d6b1247c731
               , RockShoreWaterHaulingCompany Nothing BulletEnergyServices ed0c7e53-f510-45c5-a024-5d6b1247c731
               , RockShoreWaterHaulingCompany Nothing Advantage ed0c7e53-f510-45c5-a024-5d6b1247c731
               , RockShoreWaterHaulingCompany Nothing ArkomaTanks ed0c7e53-f510-45c5-a024-5d6b1247c731
               , RockShoreWaterHaulingCompany Nothing BasicEnergyServices ed0c7e53-f510-45c5-a024-5d6b1247c731
               , RockShoreWaterHaulingCompany Nothing CottonwoodDrilling ed0c7e53-f510-45c5-a024-5d6b1247c731
               , RockShoreWaterHaulingCompany Nothing DalesTankService ed0c7e53-f510-45c5-a024-5d6b1247c731
               , RockShoreWaterHaulingCompany Nothing FluidServices ed0c7e53-f510-45c5-a024-5d6b1247c731
               , RockShoreWaterHaulingCompany Nothing GandCConstruction ed0c7e53-f510-45c5-a024-5d6b1247c731
               , RockShoreWaterHaulingCompany Nothing HammandPhillipsService ed0c7e53-f510-45c5-a024-5d6b1247c731
               , RockShoreWaterHaulingCompany Nothing HullsOilfieldService ed0c7e53-f510-45c5-a024-5d6b1247c731
               , RockShoreWaterHaulingCompany Nothing JNSTrucking ed0c7e53-f510-45c5-a024-5d6b1247c731
               , RockShoreWaterHaulingCompany Nothing KleenOilfieldService ed0c7e53-f510-45c5-a024-5d6b1247c731
               , RockShoreWaterHaulingCompany Nothing LaurcoEnergies ed0c7e53-f510-45c5-a024-5d6b1247c731
               , RockShoreWaterHaulingCompany Nothing MSMEnvironmental ed0c7e53-f510-45c5-a024-5d6b1247c731
               , RockShoreWaterHaulingCompany Nothing Nabors ed0c7e53-f510-45c5-a024-5d6b1247c731
               , RockShoreWaterHaulingCompany Nothing RHRServices ed0c7e53-f510-45c5-a024-5d6b1247c731
               , RockShoreWaterHaulingCompany Nothing SandHTankService ed0c7e53-f510-45c5-a024-5d6b1247c731
               , RockShoreWaterHaulingCompany Nothing SandM ed0c7e53-f510-45c5-a024-5d6b1247c731
               , RockShoreWaterHaulingCompany Nothing TestCompany ed0c7e53-f510-45c5-a024-5d6b1247c731]
rockShoreLogo :: Logo
rockShoreLogo = Logo "" [LogoPath . PathAttribute $ "'RockShoreEnergy.png'"  ]
