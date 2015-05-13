module Kiosk.Backend.Form.Element.Item.TableLeftHeader (TableLeftHeader(..)) where        

import qualified Data.Text as T

-- Header of table on Left
data TableLeftHeader = TableLeftHeader {
	_getTableLeftHeader :: T.Text
} deriving (Show)
