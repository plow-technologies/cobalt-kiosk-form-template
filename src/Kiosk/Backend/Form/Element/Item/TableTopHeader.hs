module Kiosk.Backend.Form.Element.Item.TableTopHeader (TableTopHeader(..)) where        

import qualified Data.Text as T

 -- Header of table on top
data TableTopHeader = TableTopHeader {
  _getTableTopHeader :: T.Text
} deriving (Show)
