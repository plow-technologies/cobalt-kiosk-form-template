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
      (renderOnpingForm defaultForm) `shouldBe` expectedString
   where
    expectedString = "<form><company width='12'>Hull's Oilfield LLC</company><address width='12'>PO Box 130 Wilson, Oklahoma 73463\n886-849-5483\nAnswering Service 580-220-9936</address><constant type='Company' indexable='True'>Black Watch</constant><row width='12'><item width='12'><label width='12'>Legal Dest</label> <input width='12' type='text' indexable='True' maxd='150.0' mind='0.0'></input></item></row> <row width='12'><item ><radio><option >Pit Water</option><option-qualifier ><label >Amount</label> <input width='12' type='text' indexable='True'></input></option-qualifier></radio></item></row></form>"

cobaltKioskForm :: String -> Form
cobaltKioskForm waterHaulingName = Form cobaltEnvironmentalSolutions cobaltAddress defaultLogo defaultPhone [createWaterHauler "Black Watch"] cobaltFormBody

cobaltEnvironmentalSolutions :: Company
cobaltEnvironmentalSolutions  = Company "Cobalt Environmental Solutions LLC" [CompanyWidth $ WidthAttribute (12::Int) ]

cobaltAddress:: Address
cobaltAddress= Address "PO Box 130 Wilson, Oklahoma 73463\n886-849-5483\nAnswering Service 580-220-9936" [AddressWidth $ WidthAttribute (12::Int)]

               

createWaterHauler :: Text -> Constant
createWaterHauler hauler = Constant hauler  [ ConstantAttributeType "'Company'"
                                            , ConstantAttributeIndexable $ IndexableAttribute True ]                   

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
    dateRow  = generateInputRowText "Date"
    timeInRow  = generateInputRowText "Time In"
    signatureRow  = generateInputRowText "Driver Signature"









waterTypeRadioRow :: Row 
waterTypeRadioRow = Row [waterTypeRadio] [RowWidth $ WidthAttribute (12::Int)]            

-- waterTypeRadio  :: Item

waterTypeRadio  = Item [ItemRadio . generateRadio $ options ] []                     
   where
     options = [generateOption "Produced Water" ]


generateLabelRow labelText = Row [generateLabelItem labelText] [RowWidth $ WidthAttribute (12::Int)]                   
generateLabelItem labelText = Item [ItemLabel . generateLabel $ labelText ] [ItemWidth $ WidthAttribute (12::Int)]


-- Input Text
generateInputRowText labelText = Row [generateInputItemText labelText] [RowWidth $ WidthAttribute (12::Int)]

generateInputItemText  labelText = Item [ItemLabel . generateLabel $ labelText
                                                    , ItemInput fullDefaultInputText] [ItemWidth $ WidthAttribute (12::Int)]

fullDefaultInputText :: Input
fullDefaultInputText = Input fullDefaultInputTypeText fullDefaultInputAttributesList

fullDefaultInputTypeText :: InputType
fullDefaultInputTypeText = InputTypeText $ InputText (""::Text)

-- Input Signature
generateInputRowSignature labelText = Row [generateInputItemSignature labelText] [RowWidth $ WidthAttribute (12::Int)]

generateInputItemSignature  labelText = Item [ItemLabel . generateLabel $ labelText
                                                    , ItemInput fullDefaultInputSignature] [ItemWidth $ WidthAttribute (12::Int)]

fullDefaultInputSignature :: Input
fullDefaultInputSignature = Input fullDefaultInputTypeSignature fullDefaultInputAttributesList

fullDefaultInputTypeSignature :: InputType
fullDefaultInputTypeSignature = InputTypeSignature $ Signature ""

-- | Radio

fullDefaultInputAttributesList :: [InputAttribute]
fullDefaultInputAttributesList = [wAttr, tAttr, ixAttr]
              where wAttr = InputWidth $ WidthAttribute (12::Int)
                    ixAttr = InputIndexable $ IndexableAttribute True
                    tAttr = InputType $ InputTypeAttribute fullDefaultInputTypeText

fullDefaultOptionQualifier :: OptionQualifier
fullDefaultOptionQualifier = OptionQualifier fullDefaultQualifierChoices []

fullDefaultQualifierChoices :: [QualifierChoices]
fullDefaultQualifierChoices = [QualifierLabel ( Label "Amount" [])
                          ,QualifierInput fullDefaultQualifierInput]

fullDefaultQualifierInput :: Input
fullDefaultQualifierInput = Input dit dia
 where 
   dit = InputTypeDouble . InputDouble $ 0.0
   dia = [wAttr, tAttr, ixAttr,minAttr,maxAttr]
   minAttr = InputMinDouble $ MinAttributeDouble (0.0::Double)
   maxAttr = InputMaxDouble $ MaxAttributeDouble (150.0::Double)   
   wAttr = InputWidth $ WidthAttribute (12::Int)
   ixAttr = InputIndexable $ IndexableAttribute True
   tAttr = InputType $ InputTypeAttribute dit

generateLabel labelText = Label labelText [LabelWidth $ WidthAttribute (12::Int)]

generateRadio options = Radio options [fullDefaultOptionQualifier]

generateOption :: Text -> Option 
generateOption optionText = Option optionText []

