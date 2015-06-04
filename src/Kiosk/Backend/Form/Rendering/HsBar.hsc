{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP                      #-}

module Kiosk.Backend.Form.Rendering.HsBar where

import Foreign
import Foreign.C
import Control.Applicative
import Control.Monad

#include "bar.h"
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

foreign export ccall "freePointerSetInHaskell" free :: Ptr a -> IO ()

data AttributeToC = AttributeToC {
    nameToC :: Ptr TextToC
  , valToC :: Ptr TextToC
} deriving Show

instance Storable AttributeToC where
    sizeOf    _ = #{size attribute}
    alignment _ = #{alignment attribute}
    poke p attribute = do
        #{poke attribute, name} p $  nameToC attribute
        #{poke attribute, val} p $  valToC attribute
    peek p = return AttributeToC
        `ap` (#{peek attribute,name} p)
        `ap` (#{peek attribute,val} p)

