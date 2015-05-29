{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Kiosk.Backend.Form.Attribute ( AttributeClass(..)
                                    , Attribute(..)
                                    , wrongAttrResponse) where

import           Data.Text    (Text)
import qualified Data.Text    as T
import           GHC.Generics (Generic)

data Attribute = Attribute {
	  name :: Text
	, val  :: Text 
} deriving (Generic, Show)

-- Type Class for Attributes
class AttributeClass a where
  toAttribute :: a -> Attribute
  fromAttribute :: Attribute -> Either T.Text a

-- | Utility Function
wrongAttrResponse :: T.Text -> T.Text -> Either T.Text a
wrongAttrResponse correct other = Left $ T.concat ["Not ", correct, "attribute, recieved --> ", other]
