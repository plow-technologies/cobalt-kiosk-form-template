{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Kiosk.Backend.Form.ParserSpec (main, spec) where

import Kiosk.Backend.Form
import Kiosk.Backend.Form.Attribute.Indexable
import Kiosk.Backend.Form.Parsers
import Text.Trifecta ( parseString
                     , Result(..)
                     , parseTest)
import Text.Trifecta.Delta (Delta(..))                     
import Language.Haskell.TH (nameBase )
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe (nameBase 'genericAttributeDecoder) $ do    
   it "should turn Attributes into something specific" $ 
     True `shouldBe` True
  describe (nameBase 'inputParser ) $ do
    it "should parse <input> elements" $ do    
     let (Success parsedInputDouble) = (parseString inputParser  (Columns 0 0) "<input type='double' indexable='True'>3.3</input>")
     True `shouldBe` True
     
--     expectedInputDouble `shouldBe` parsedInputDouble


-- attributeList = [Attribute {name = "type", val = "double"}
--                 ,Attribute {name = "indexable", val = "True"}]                                              

-- expectedInputDouble :: Input
-- expectedInputDouble = Input {_getInput = InputTypeText (InputText {_getInputText = ""}), _inputAttrib = [InputType (InputTypeAttribute {_getTypeName = InputTypeDouble (InputDouble {_getInputDouble = 0.0})}),InputIndexable (IndexableAttribute {_getIndexable = True})]}

-- expectedString :: String
-- expectedString = "<form><company width='12'>Hull's Oilfield LLC</company><address width='12'>PO Box 130 Wilson, Oklahoma 73463\n886-849-5483\nAnswering Service 580-220-9936</address><constant type='Company' indexable='True'>Black Watch</constant><row width='12'><item width='12'><label width='12'>Legal Dest</label> <input width='12' type='text' indexable='True' maxd='150.0' mind='0.0'></input></item></row> <row width='12'><item ><radio><option >Pit Water</option><option-qualifier ><label >Amount</label> <input width='12' type='text' indexable='True'></input></option-qualifier></radio></item></row></form>"



