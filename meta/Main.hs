{-# LANGUAGE TemplateHaskell #-}

import Language.Haskell.TH
import Kiosk.Backend.Form.Rendering.Metaprogramming.CStruct
import Kiosk.Backend.Form

-- Attribute
vals = [ $(getTypeAndRecords ''Attribute)
       , $(getTypeAndRecords ''ActionAttribute)
       , $(getTypeAndRecords ''IndexableAttribute)
       , $(getTypeAndRecords ''MaxAttributeDouble)
       , $(getTypeAndRecords ''MinAttributeDouble)
       , $(getTypeAndRecords ''PathAttribute)
       , $(getTypeAndRecords ''WidthAttribute)
       , $(getTypeAndRecords ''AddressAttributes)
       , $(getTypeAndRecords ''Address)
       ]

x = $(getTypeAndRecords ''Address)
z = $(getTypeAndRecords ''AddressAttributes)

main :: IO ()
main = do
  -- include/bar.h
  -- cbits/bar.c
  -- src/Kiosk/Backend/Form/Rendering/HsBar.hsc
  createHeaderAndHSC "bar" "HsBar" "Kiosk.Backend.Form.Rendering.HsBar" vals
  -- print vals
  print x

  -- print "Hello from main"
  print z