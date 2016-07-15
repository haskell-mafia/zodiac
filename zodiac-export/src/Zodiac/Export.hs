{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Zodiac.Export(
  )where

import           Foreign.C.Types (CUChar)
import           Foreign.Ptr (Ptr)

import           System.IO (IO)

import           Zodiac.Export.Key

foreign export ccall "_z_tsrp_gen_key_id" genKeyId :: Ptr CUChar -> IO ()

foreign export ccall "_z_tsrp_gen_symmetric_key" genSymmetricKey :: Ptr CUChar -> IO ()

