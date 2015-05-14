
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}

module Kiosk.Backend.Form.Attribute ( AttributeClass(..)
                                    , Attribute(..)
                                    , wrongAttrResponse) where

import           GHC.Generics           (Generic)
import qualified Data.Text as T

data Attribute = Attribute {
	name :: T.Text,
	val  :: T.Text
} deriving (Generic, Show)

-- Type Class for Attributes
class AttributeClass a where
  toAttribute :: a -> Attribute
  fromAttribute :: Attribute -> Either T.Text a

 -- | Utility Function
wrongAttrResponse :: T.Text -> T.Text -> Either T.Text a
wrongAttrResponse correct other = Left $ T.concat ["Not ", correct, "attribute, recieved --> ", other]
