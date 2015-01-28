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

-- TODO: 
-- Add Phone
-- Add Logo 
defaultPhone :: Phone
defaultPhone = Phone "580-229-0067" [PhoneWidth $ WidthAttribute (12::Int) ]   

cobaltKioskForm :: String -> Form
cobaltKioskForm waterHaulingName = Form cobaltEnvironmentalSolutions cobaltAddress [createWaterHauler "Black Watch"] cobaltFormBody

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
    truckNumberRow  = generateInputRow "Truck #"
    permitNumberRow  = generateInputRow "Water Hauling Permit #"
    leaseInfoRow  = generateLabelRow "Lease Information"
    leaseNameRow = generateInputRow "Name of Lease Operator"
    waterTypeAndAmountRow  = waterTypeRadioRow
    dateRow  = generateInputRow "Date"
    timeInRow  = generateInputRow "Time In"
    signatureRow  = generateInputRow "Driver Signature"







-- generateInputRow :: [Row]
generateInputRow labelText = Row [generateInputItem labelText] [RowWidth $ WidthAttribute (12::Int)]
                             

waterTypeRadioRow :: Row 
waterTypeRadioRow = Row [waterTypeRadio] [RowWidth $ WidthAttribute (12::Int)]            

-- waterTypeRadio  :: Item

waterTypeRadio  = Item [ItemRadio . generateRadio $ options ] []                     
   where
     options = [generateOption "Produced Water" ]


--fullDefaultInputItem :: Item


generateLabelRow labelText = Row [generateLabelItem labelText] [RowWidth $ WidthAttribute (12::Int)]                   
generateLabelItem labelText = Item [ItemLabel . generateLabel $ labelText ] [ItemWidth $ WidthAttribute (12::Int)]

generateInputItem  labelText = Item [ItemLabel . generateLabel $ labelText
                                                , ItemInput fullDefaultInput] [ItemWidth $ WidthAttribute (12::Int)]


fullDefaultOptionQualifier :: OptionQualifier
fullDefaultOptionQualifier = OptionQualifier fullDefaultQualifierChoices []

fullDefaultQualifierChoices :: [QualifierChoices]
fullDefaultQualifierChoices = [QualifierLabel ( Label "Amount" [])
                          ,QualifierInput fullDefaultQualifierInput]

fullDefaultQualifierInput :: Input
fullDefaultQualifierInput = Input dit dia
 where 
   dit = InputTypeText . InputText $ "" 
   dia = [wAttr, tAttr, ixAttr]
   wAttr = InputWidth $ WidthAttribute (12::Int)
   ixAttr = InputIndexable $ IndexableAttribute True
   tAttr = InputType $ InputTypeAttribute dit

generateLabel labelText = Label labelText [LabelWidth $ WidthAttribute (12::Int)]

generateRadio options = Radio options [fullDefaultOptionQualifier]

generateOption :: Text -> Option 
generateOption optionText = Option optionText []


fullDefaultInput :: Input
fullDefaultInput = Input fullDefaultInputType fullDefaultInputAttributesList

fullDefaultInputType :: InputType
fullDefaultInputType = InputTypeText $ InputText (""::Text)

fullDefaultInputAttributesList :: [InputAttribute]
fullDefaultInputAttributesList = [wAttr, tAttr, ixAttr,maxAttr,minAttr]
              where wAttr = InputWidth $ WidthAttribute (12::Int)
                    ixAttr = InputIndexable $ IndexableAttribute True
                    minAttr = InputMinDouble $ MinAttributeDouble (0.0::Double)
                    maxAttr = InputMaxDouble $ MaxAttributeDouble (150.0::Double)
                    tAttr = InputType $ InputTypeAttribute fullDefaultInputType
