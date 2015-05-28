{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import           Kiosk.Backend.Form
import           Kiosk.Backend.Form.Generator.Cobalt
import           Kiosk.Backend.Form.Generator.RockShore

import           Control.Applicative
import           Data.Attoparsec.Text
import           System.Exit                            (exitFailure)

import           Control.Lens
import qualified Data.Text                              as T
import           Data.Traversable
import           Regex.Genex
import           Test.Hspec
import           Test.QuickCheck


-- Lenses to make things easier for testing

makeLenses ''Input
makeLenses ''InputDouble
makeLenses ''InputInt
makeLenses ''InputText
makeLenses ''Signature
makePrisms ''InputType
makePrisms ''InputAttribute
makeLenses ''IndexableAttribute

testParser  :: Show a => Parser a ->
               T.Text ->
               (a -> Bool) -> Expectation
testParser parser inputText validator = runValidator
  where
      (Right rslt) = parseOnly parser inputText
      runValidator = shouldSatisfy rslt validator

testInputTypeDouble = InputTypeDouble

andNotNull lst = and lst && (not.null $ lst)


main :: IO ()
main = hspec $ do
 describe "inputParser" $ do
  it "should parse various indexable types correctly" $ do
  -- index forces the type to be InputTypeText, value is lost
    testParser inputParser "<input type='double' indexable='True'>3.3</input>"
                           (\i -> (i ^.. getInput._InputTypeDouble & null & not) &&
                                  (i ^.. getInput._InputTypeDouble.getInputDouble <&> (== 3.3) & andNotNull ) &&
                                  (i ^.. inputAttrib. traverse . _InputIndexable. getIndexable & andNotNull))

    testParser inputParser "<input type='text' indexable='True'>Plowtech</input>"
                           (\i -> (i ^.. getInput._InputTypeText & null & not) &&
                                  (i ^.. getInput._InputTypeText.getInputText <&> (== "Plowtech") & andNotNull ) &&
                                  (i ^.. inputAttrib. traverse . _InputIndexable. getIndexable & andNotNull))
    testParser inputParser "<input type='signature' indexable='True'>as9d8j2l3kfaoiu1239h</input>"
                           (\i -> (i ^.. getInput._InputTypeSignature.signature <&> (== "as9d8j2l3kfaoiu1239h") & andNotNull) &&
                                  (i ^.. inputAttrib. traverse . _InputIndexable. getIndexable & andNotNull))
    testParser inputParser "<input type='int' indexable='True'>1234</input>"
                           (\i -> (i ^.. getInput._InputTypeInt.getInputInt <&> (== 1234) & andNotNull) &&
                                  (i ^.. inputAttrib. traverse . _InputIndexable. getIndexable & andNotNull))
  it "should parse various input types correctly" $ do
  -- without indexable
    testParser inputParser "<input type='double'>3.3</input>" (const True)
    testParser inputParser "<input type='text'>Plowtech</input>" (const True)
    testParser inputParser "<input type='signature'>as9d8j2l3kfaoiu1239h</input>" (const True)
    testParser inputParser "<input type='int'>1234</input>" (const True)
    testParser inputParser "<input type='int'>1234</input>" (const True)
  it "should parse various buttons and labels" $ do
  -- button and label
    testParser buttonParser "<button w(const True)th='12' action='sendJson'></button>" (const True)
    testParser labelParser "<label w(const True)th='12'>Legal Dest</label>" (const True)
--    print $ (renderOnpingForm . cobaltKioskForm $ "Black Watch")
  it "should parse various form address logo company stuff" $ do
    testParser parseForm "<entry><form><address>Rockshore</address></form></entry>" (const True)
    testParser parseForm "<entry><form><company>Rockshore</company></form></entry>" (const True)
    testParser parseForm "<entry><form><company abc='1234'>Rockshore </company> </form></entry>" (const True)
    testParser parseCompanyElement "<company>Rockshore</company>" (const True)

    testParser parseCompanyElement "<company>Rockshore</company>" (const True)
    testParser parseCompanyElement "<company wrong='shouldbreak'>Rockshore</company>" (const True)
    testParser parseLogoElement "<logo path='home'></logo>" (const True)
    testParser parseLogoElement "<logo path='home'></logo>" (const True)
  it "should parse a row and entry correctly" $ do
    testParser parseRow "<row><item></item></row>" (const True)
    testParser parseRow "<row><item width='12'><label width='12'>Driver's Signature</label><input type='signature' width='12'></input></item></row>" (const True)
    testParser parseForm "<entry><form><company>Rockshore</company></form></entry>" (const True)
    testParser parseForm "<entry><form><company>Rockshore</company><company>Rockshore</company><address>72341234</address></form></entry>" (const True)
    testParser parseForm "<entry><form></form></entry>" (const True)
    testParser parseForm "<entry><form><row><item><label>Just a label</label></item></row></form></entry>" (const True)
  it "should parse items of various kinds " $ do
    testParser parseItemInput "<item width='12'><label width='12'>Well Amount</label><input type='text' width='12'></input></item>" (const True)
    testParser parseItemInput "<item width='12'><label width='12'>Driver's Signature</label><input type='signature' width='12'></input></item>" (const True)
    testParser parseItemRadio "<item width='12'><radio><label width='12'>Choices</label><option>1</option></radio></item>" (const True)

    testParser parseForm "<entry><form><company>Rockshore</company><address>72341234</address><logo path='logo.png'></logo><phone>918-918-9188</phone><row><item width='12'><label width='12'>Driver's Signature</label><input type='signature' width='12'></input></item></row><row><item width='12'><radio><label width='12'>Choices</label><option>1</option></radio></item></row></form></entry>" (const True)


-- <option>2</option><option>3</option>
--updateThisThing i n = post ("http://alarm.plowtech.net:2834/form/update?formid=" ++ (show i)) (encode.cobaltKioskForm $ n)
--updateAllFormsForAllCompanies = traverse (uncurry updateThisThing)   currentForms
--expectedString
--   where
--    expectedString = "<form><company width='12'>Cobalt Environmental Solutions LLC</company><address width='12'>PO Box 130 Wilson, Oklahoma 73463\n886-849-5483\nAnswering Service 580-220-9936</address><logo path='Cobalt.png'></logo><phone width='12'>580-229-0067</phone><constant type='Water Hauling Company' indexable='True'>Black Watch</constant><row ><item ><label width='12'>Truck #</label> <input type='text' indexable='True'></input></item></row> <row ><item ><label width='12'>Water Hauling Permit #</label> <input type='text' indexable='True'></input></item></row> <row ><item ><label width='12'>Lease Information</label></item></row> <row ><item ><label width='12'>Name of Lease Operator</label> <input type='text' indexable='True'></input></item></row> <row ><item ><radio><label width='12'>Type of Water Hauled</label><option >Produced Water</option> <option >Pit Water</option> <option >Fresh Water</option> <option >Flowback Water</option><option-qualifier ><label >Amount</label> <input type='double' indexable='True' mind='0.0' maxd='150.0'>0.0</input></option-qualifier></radio></item></row> <row ><item ><label width='12'>Date</label> <input type='date'></input></item></row> <row ><item ><label width='12'>Time In</label> <input type='time'></input></item></row> <row ><item ><label width='12'>Driver Signature</label> <input type='signature'></input></item></row></form>"

currentForms = [(0,"Big Star Trucking")
               ,(1,"Bullet Energy Services")
               ,(2,"C & J Trucking")
               ,(3,"Big Mac Tank Trucks")
               ,(4,"Brady Welding and Machine Shop")
               ,(5,"Kleen Oilfield Services")
               ,(6,"B & C Backhoe & Transports")
               ,(7,"Forsythe Oilfield ")
               ,(8,"Hulls Oilfield")
               ,(9,"South Central Oilfield Services")
               ,(10, "Top-O-Texas")
               ,(11,"Mitchell Tank Truck Services")
               ,(12,"Fluid Services")
               ,(13,"Davenport Oilfield Services")
               ,(14,"Test Company")]


cobaltKioskForm :: T.Text -> Form
cobaltKioskForm waterHaulingName = Form cobaltEnvironmentalSolutions cobaltAddress defaultLogo defaultPhone [createWaterHauler waterHaulingName] cobaltFormBody

cobaltEnvironmentalSolutions :: Company
cobaltEnvironmentalSolutions  = Company "Cobalt Environmental Solutions LLC" [CompanyWidth $ WidthAttribute (12::Int) ]

cobaltAddress:: Address
cobaltAddress= Address "PO Box 130 Wilson, Oklahoma 73463\n886-849-5483\n" [AddressWidth $ WidthAttribute (12::Int)]



createWaterHauler :: T.Text -> Constant
createWaterHauler hauler = Constant hauler  [ ConstantAttributeType "'Water Hauling Company'"
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
                                                    , ItemInput fullDefaultInputDate] []

fullDefaultInputDate :: Input
fullDefaultInputDate = Input fullDefaultInputTypeDate [InputType InputTypeAttributeDate]

fullDefaultInputTypeDate :: InputType
fullDefaultInputTypeDate = InputTypeDate $ (InputDate "")

-- Input Time
generateInputRowTime :: T.Text -> Row
generateInputRowTime labelTime = Row [generateInputItemTime labelTime] []

generateInputItemTime :: T.Text -> Item
generateInputItemTime  labelTime = Item [ItemLabel . generateLabel $ labelTime
                                                    , ItemInput fullDefaultInputTime] []

fullDefaultInputTime :: Input
fullDefaultInputTime = Input fullDefaultInputTypeTime [InputType InputTypeAttributeTime]

fullDefaultInputTypeTime :: InputType
fullDefaultInputTypeTime = InputTypeTime $ (InputTime "")

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

generateLabel :: T.Text -> Label
generateLabel labelText = Label labelText [LabelWidth $ WidthAttribute (12::Int)]

generateRadio :: T.Text -> [Option] -> Radio
generateRadio labelText options = Radio (generateLabel labelText) options [fullDefaultOptionQualifier]

generateOption :: T.Text -> Option
generateOption optionText = Option optionText []
