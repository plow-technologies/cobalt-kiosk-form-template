{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Kiosk.Backend.Form.ParserSpec (main, spec) where

import Kiosk.Backend.Form
import Language.Haskell.TH (nameBase )
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe (nameBase 'renderOnpingForm) $ do
    it "should render a form as described below" $ do
      (renderOnpingForm defaultForm) `shouldBe` expectedString
   where
    expectedString = "<form><company width='12'>Hull's Oilfield LLC</company><address width='12'>PO Box 130 Wilson, Oklahoma 73463\n886-849-5483\nAnswering Service 580-220-9936</address><constant type='Company' indexable='True'>Black Watch</constant><row width='12'><item width='12'><label width='12'>Legal Dest</label> <input width='12' type='text' indexable='True' maxd='150.0' mind='0.0'></input></item></row> <row width='12'><item ><radio><option >Pit Water</option><option-qualifier ><label >Amount</label> <input width='12' type='text' indexable='True'></input></option-qualifier></radio></item></row></form>"
