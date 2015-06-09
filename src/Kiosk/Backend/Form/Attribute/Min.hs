{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Kiosk.Backend.Form.Attribute.Min (MinAttributeDouble(..)) where

import           Data.Aeson                   (FromJSON, ToJSON)
import qualified Data.Text                    as T
import           GHC.Generics                 (Generic)
import           Kiosk.Backend.Form.Attribute
import           Text.Read                    (readMaybe)

data MinAttributeDouble = MinAttributeDouble {
    _getGenericMinAmt :: Double
} deriving (Show, Ord, Eq, Generic)


instance ToJSON MinAttributeDouble where
instance FromJSON MinAttributeDouble where

instance AttributeClass MinAttributeDouble where
   toAttribute (MinAttributeDouble d) = Attribute "mind" (T.pack ("'" ++ show d ++ "'"))
   fromAttribute (Attribute "mind" m) = case readMaybe (T.unpack m) of
                                    (Just m') -> Right (MinAttributeDouble m')
                                    Nothing -> Left $ T.concat ["MinAttributeDouble value not parsing -->",m]
   fromAttribute (Attribute other _) = wrongAttrResponse "mind" other
