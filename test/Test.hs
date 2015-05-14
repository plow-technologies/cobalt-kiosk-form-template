{-# LANGUAGE OverloadedStrings #-}

module Main where

import Kiosk.Backend.Form
import Kiosk.Backend.Form.Generator.Cobalt
import Kiosk.Backend.Form.Generator.RockShore

import Control.Applicative
import Data.Attoparsec.Text
import System.Exit (exitFailure) 

import qualified Data.Text as T
import Data.Traversable

-- I found this commented out in the old version
-- not sure if it is still supposed to look like this
-- "<input type='double' indexable='True'>0.0</input>"
-- expectedInputDouble :: Input
-- expectedInputDouble = Input {_getInput = InputTypeText (InputText {_getInputText = ""}), _inputAttrib = [InputType (InputTypeAttribute {_getTypeName = InputTypeDouble (InputDouble {_getInputDouble = 0.0})}),InputIndexable (IndexableAttribute {_getIndexable = True})]}


{- current output, does this look right?
Right (Input {_getInput = InputTypeDouble (InputDouble {_getInputDouble = 3.3}), _inputAttrib = [InputType InputTypeAttributeDouble,InputIndexable (IndexableAttribute {_getIndexable = True})]})
Right (Input {_getInput = InputTypeText (InputText {_getInputText = "Plowtech"}), _inputAttrib = [InputType InputTypeAttributeText,InputIndexable (IndexableAttribute {_getIndexable = True})]})
Right (Input {_getInput = InputTypeSignature (Signature {_signature = "as9d8j2l3kfaoiu1239h"}), _inputAttrib = [InputType InputTypeAttributeSignature,InputIndexable (IndexableAttribute {_getIndexable = True})]})
Right (Input {_getInput = InputTypeInt (InputInt {_getInputInt = 1234}), _inputAttrib = [InputType InputTypeAttributeInt,InputIndexable (IndexableAttribute {_getIndexable = True})]})

Right (Input {_getInput = InputTypeDouble (InputDouble {_getInputDouble = 3.3}), _inputAttrib = [InputType InputTypeAttributeDouble]})
Right (Input {_getInput = InputTypeText (InputText {_getInputText = "Plowtech"}), _inputAttrib = [InputType InputTypeAttributeText]})
Right (Input {_getInput = InputTypeSignature (Signature {_signature = "as9d8j2l3kfaoiu1239h"}), _inputAttrib = [InputType InputTypeAttributeSignature]})
Right (Input {_getInput = InputTypeInt (InputInt {_getInputInt = 1234}), _inputAttrib = [InputType InputTypeAttributeInt]})

Right (Button {_getButtonText = "", _buttonAttrib = [ButtonWidth (WidthAttribute {_getWidth = 12}),ButtonAction (ActionAttribute {_getFunctionName = "sendJson"})]})
Right (Label {_getLabelText = "Legal Dest", _labelAttrib = [LabelWidth (WidthAttribute {_getWidth = 12})]})
-}
main :: IO ()
main = do
	-- with indexable
	-- index forces the type to be InputTypeText, value is lost
	print $ parseOnly inputParser "<input type='double' indexable='True'>3.3</input>"
	print $ parseOnly inputParser "<input type='text' indexable='True'>Plowtech</input>"
	print $ parseOnly inputParser "<input type='signature' indexable='True'>as9d8j2l3kfaoiu1239h</input>"
	print $ parseOnly inputParser "<input type='int' indexable='True'>1234</input>"
	
	-- without indexable
	print $ parseOnly inputParser "<input type='double'>3.3</input>"
	print $ parseOnly inputParser "<input type='text'>Plowtech</input>"
	print $ parseOnly inputParser "<input type='signature'>as9d8j2l3kfaoiu1239h</input>"
	print $ parseOnly inputParser "<input type='int'>1234</input>"
	
	-- button and label
	print $ parseOnly buttonParser "<button width='12' action='sendJson'></button>"
	print $ parseOnly labelParser "<label width='12'>Legal Dest</label>"
	print $ (renderOnpingForm . cobaltKioskForm $ "Black Watch")
	exitFailure


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
