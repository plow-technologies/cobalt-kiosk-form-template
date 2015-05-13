{-# LANGUAGE OverloadedStrings #-}

module Kiosk.Backend.Form.Attribute.Min (MinAttributeDouble(..)) where

import Kiosk.Backend.Form.Attribute
import qualified Data.Text as T
import Text.Read   (readMaybe)

data MinAttributeDouble = MinAttributeDouble {
    _getGenericMinAmt  :: Double
} deriving (Show, Ord, Eq)

instance AttributeClass MinAttributeDouble where
   toAttribute (MinAttributeDouble d) = Attribute "mind" (T.pack ("'" ++ show d ++ "'"))
   fromAttribute (Attribute "mind" m) = case readMaybe (T.unpack m) of
                                    (Just m') -> Right (MinAttributeDouble m')
                                    Nothing -> Left $ T.concat ["MinAttributeDouble value not parsing -->",m]
   fromAttribute (Attribute other _) = wrongAttrResponse "mind" other