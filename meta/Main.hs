{-# LANGUAGE TemplateHaskell #-}

import Language.Haskell.TH
import Kiosk.Backend.Form.Rendering.Metaprogramming.CStruct
import Kiosk.Backend.Form.Attribute

-- Attribute
a = $(getTypeAndRecords ''Attribute)

main :: IO ()
main = do
  -- include/bar.h
  -- cbits/bar.c
  -- src/Kiosk/Backend/Form/Rendering/HsBar.hsc
  createHeaderAndHSC "bar" "HsBar" "Kiosk.Backend.Form.Rendering.HsBar" [a]
  print "Hello from main"