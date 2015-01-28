{- |
Module      :  Kiosk.Backend.Form
Description :  Form Types for Kiosk
Copyright   :  (c) <Plow Technology 2014>
License     :  <MIT>
Maintainer  :  <lingpo.huang@plowtech.net>
Stability   :  unstable
Portability :  portable
<Haskell Types for the form sending from the SWIF Application>
         
Rendering the default Form Produces:
          
>>> renderOnpingForm defaultForm
<form>
    <company width="12">Hull's Oilfield LLC</company>
    <address width="12">PO Box 130 Wilson, Oklahoma 73463\n886-849-5483\nAnswering Service 580-220-9936</address>
    <row width="12">
        <item width="12">
            <label width="12">Legal Dest</label>
            <input width="12" type="text" indexable="True"/>
        </item>
    </row>
</form>

-}

module Kiosk.Backend.Form ( module Kiosk.Backend.Form
                          ) where

import           Kiosk.Backend.Form.Attribute as Kiosk.Backend.Form
import           Kiosk.Backend.Form.Attribute.Width as Kiosk.Backend.Form
import           Kiosk.Backend.Form.Attribute.Action as Kiosk.Backend.Form
import           Kiosk.Backend.Form.Attribute.Indexable as Kiosk.Backend.Form
import           Kiosk.Backend.Form.Attribute.Min as Kiosk.Backend.Form
import           Kiosk.Backend.Form.Attribute.Max as Kiosk.Backend.Form
import           Kiosk.Backend.Form.Element   as Kiosk.Backend.Form
import           Kiosk.Backend.Form.Parsers   as Kiosk.Backend.Form
import           Kiosk.Backend.Form.Rendering as Kiosk.Backend.Form
