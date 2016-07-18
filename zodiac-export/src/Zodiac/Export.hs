{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Zodiac.Export(
  )where

import           Foreign.C.Types (CTime(..), CChar(..), CUChar(..), CSize(..), CInt(..))
import           Foreign.Ptr (Ptr)

import           System.IO (IO)

import           Zodiac.Export.Key
import           Zodiac.Export.TSRP

foreign export ccall "_z_tsrp_gen_key_id" genKeyId :: Ptr CUChar -> IO ()

foreign export ccall "_z_tsrp_gen_symmetric_key" genSymmetricKey :: Ptr CUChar -> IO ()

foreign export ccall "_z_tsrp_verify" verifyRawRequest' :: Ptr CChar
                                                        -> Ptr CChar
                                                        -> Ptr CChar
                                                        -> CSize
                                                        -> CTime
                                                        -> IO CInt

