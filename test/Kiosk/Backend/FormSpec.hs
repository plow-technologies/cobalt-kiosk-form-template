{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Kiosk.Backend.FormSpec (main, spec) where

import Kiosk.Backend.Form
import Language.Haskell.TH (nameBase )
import Test.Hspec

import Data.Text

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe (nameBase 'renderOnpingForm) $ do
    it "should render a form as described below" $ do
      (renderOnpingForm . cobaltKioskForm $ "Black Watch") `shouldBe` expectedString
   where
    expectedString = "<form><company width='12'>Cobalt Environmental Solutions LLC</company><address width='12'>PO Box 130 Wilson, Oklahoma 73463\n886-849-5483\nAnswering Service 580-220-9936</address><logo path='Cobalt.png'></logo><phone width='12'>580-229-0067</phone><constant type='Company' indexable='True'>Black Watch</constant><row ><item ><label width='12'>Truck #</label> <input type='text' indexable='True'></input></item></row> <row ><item ><label width='12'>Water Hauling Permit #</label> <input type='text' indexable='True'></input></item></row> <row ><item ><label width='12'>Lease Information</label></item></row> <row ><item ><label width='12'>Name of Lease Operator</label> <input type='text' indexable='True'></input></item></row> <row ><item ><radio><option >Produced Water</option><option-qualifier ><label >Amount</label> <input type='double' indexable='True' mind='0.0' maxd='150.0'>0.0</input></option-qualifier></radio></item></row> <row ><item ><label width='12'>Date</label> <input type='date'></input></item></row> <row ><item ><label width='12'>Time In</label> <input type='time'></input></item></row> <row ><item ><label width='12'>Driver Signature</label> <input type='signature'></input></item></row></form>"

cobaltKioskForm :: Text -> Form
cobaltKioskForm waterHaulingName = Form cobaltEnvironmentalSolutions cobaltAddress defaultLogo defaultPhone [createWaterHauler waterHaulingName] cobaltFormBody

cobaltEnvironmentalSolutions :: Company
cobaltEnvironmentalSolutions  = Company "Cobalt Environmental Solutions LLC" [CompanyWidth $ WidthAttribute (12::Int) ]

cobaltAddress:: Address
cobaltAddress= Address "PO Box 130 Wilson, Oklahoma 73463\n886-849-5483\nAnswering Service 580-220-9936" [AddressWidth $ WidthAttribute (12::Int)]

               

createWaterHauler :: Text -> Constant
createWaterHauler hauler = Constant hauler  [ ConstantAttributeType "'Company'"
                                            , ConstantAttributeIndexable $ IndexableAttribute True ]                   
cobaltFormBody :: [Row]
cobaltFormBody = [ truckNumberRow
                 , permitNumberRow
                 , leaseInfoRow
                 , leaseNameRow
                 , waterTypeAndAmountRow
                 , dateRow
                 , timeInRow
                 , signatureRow]
  where 
    truckNumberRow  = generateInputRowText "Truck #"
    permitNumberRow  = generateInputRowText "Water Hauling Permit #"
    leaseInfoRow  = generateLabelRow "Lease Information"
    leaseNameRow = generateInputRowText "Name of Lease Operator"
    waterTypeAndAmountRow  = waterTypeRadioRow
    dateRow  = generateInputRowDate "Date"
    timeInRow  = generateInputRowTime "Time In"
    signatureRow  = generateInputRowSignature "Driver Signature"









waterTypeRadioRow :: Row 
waterTypeRadioRow = Row [waterTypeRadio] []            


waterTypeRadio :: Item
waterTypeRadio  = Item [ItemRadio . generateRadio $ options ] []                     
   where
     options = [generateOption "Produced Water" ]

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

generateRadio :: [Option] -> Radio
generateRadio options = Radio options [fullDefaultOptionQualifier]

generateOption :: Text -> Option 
generateOption optionText = Option optionText []

