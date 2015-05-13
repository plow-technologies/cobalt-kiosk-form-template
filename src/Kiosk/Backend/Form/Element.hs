{- |
It can be confusing looking for Elements and Attributes,
The rule is Every element has a list of attributes defined along side it that
define what attributes can be used with that element but the primitive types
of these attribute definitions come from the Attribute type.
For example, 'Company' has 'CompanyAttribute', one of which is a 'WidthAttr'
-}

{-# LANGUAGE OverloadedStrings  #-}

module Kiosk.Backend.Form.Element (module Kiosk.Backend.Form.Element) where


import           Kiosk.Backend.Form.Element.Address              as Kiosk.Backend.Form.Element
import           Kiosk.Backend.Form.Element.Company              as Kiosk.Backend.Form.Element
import           Kiosk.Backend.Form.Element.Form                 as Kiosk.Backend.Form.Element
import           Kiosk.Backend.Form.Element.Phone                as Kiosk.Backend.Form.Element
import           Kiosk.Backend.Form.Element.Logo                 as Kiosk.Backend.Form.Element
import           Kiosk.Backend.Form.Element.Constant             as Kiosk.Backend.Form.Element
import           Kiosk.Backend.Form.Element.Row                  as Kiosk.Backend.Form.Element
import           Kiosk.Backend.Form.Element.Item                 as Kiosk.Backend.Form.Element
import           Kiosk.Backend.Form.Element.Item.Button          as Kiosk.Backend.Form.Element
import           Kiosk.Backend.Form.Element.Item.EmptyBlock      as Kiosk.Backend.Form.Element
import           Kiosk.Backend.Form.Element.Item.Input           as Kiosk.Backend.Form.Element
import           Kiosk.Backend.Form.Element.Item.Radio           as Kiosk.Backend.Form.Element
import           Kiosk.Backend.Form.Element.Item.Label           as Kiosk.Backend.Form.Element
import           Kiosk.Backend.Form.Element.Item.TableLeftHeader as Kiosk.Backend.Form.Element
import           Kiosk.Backend.Form.Element.Item.TableTopHeader  as Kiosk.Backend.Form.Element


{-|
Description of Simple Elements
* 'Address' The address of the company using the Kiosk
* 'Company' Name of the company using the Kiosk
* 'Form' The root element of the Kiosk Data  
* 'Row' Element that determines a break in the form flow
* 'Item' Similar to a div, but with more context given by the following types
* 'Button' Clickable Element
* 'EmptyBlock' Space Filler Element
* 'Input' Element that will be entered by user 
* 'Label' Label for an Item 
* 'TableLeftHeader' put a bold text to the left  
* 'TableTopHeader' put a bold text to the top
-}
