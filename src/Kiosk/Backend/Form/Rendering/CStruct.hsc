{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP                      #-}

module Kiosk.Backend.Form.Rendering.CStruct where

import Foreign
import Foreign.C

-- import Control.Applicative
import Control.Monad

import qualified Data.Text                    as T
import qualified Kiosk.Backend.Form.Attribute as A
-- import qualified Kiosk.Backend.Form.Element   as E

#include "foo.h"

-- Attribute to AttributeC to struct Attribute
foreign export ccall "setAttribute" setAttribute :: Ptr AttributeC -> IO ()
setAttribute :: Ptr AttributeC -> IO ()
setAttribute f = do
  newName <- newCString "abc"
  newVal <- newCString "def"
  poke f $ AttributeC newName newVal
  return ()

data AttributeC = AttributeC {
    name :: CString
  , val  :: CString
} deriving (Show)

convertAttributeToC :: A.Attribute -> IO AttributeC
convertAttributeToC att = do
  cName <- newCString $ T.unpack $ A.name att
  cVal  <- newCString $ T.unpack $ A.val  att
  return $ AttributeC cName cVal

instance Storable AttributeC where
    sizeOf    _ = #{size attribute}
    alignment _ = alignment (undefined :: CString)
    
    poke p attribute = do
        #{poke attribute, name} p $ name attribute
        #{poke attribute, val}  p $ val  attribute

    peek p = return AttributeC
              `ap` (#{peek attribute, name} p)
              `ap` (#{peek attribute, val} p)

data Foo = Foo { 
    a :: CString
  , b :: CString
  , c :: Int
} deriving Show

instance Storable Foo where
    sizeOf    _ = #{size foo}
    alignment _ = alignment (undefined :: CString)
    
    poke p foo = do
        #{poke foo, a} p $ a foo
        #{poke foo, b} p $ b foo
        #{poke foo, c} p $ c foo

    peek p = return Foo
              `ap` (#{peek foo, a} p)
              `ap` (#{peek foo, b} p)
              `ap` (#{peek foo, c} p)

foreign export ccall "free_HaskellPtr" free :: Ptr a -> IO ()

foreign export ccall "setFoo" setFoo :: Ptr Foo -> IO ()
setFoo :: Ptr Foo -> IO ()
setFoo f = do
  newA <- newCString "abc"
  newB <- newCString "def"
  poke f $ Foo newA newB 3
  return ()