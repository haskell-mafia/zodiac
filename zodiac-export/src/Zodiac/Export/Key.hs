{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Zodiac.Export.Key (
    genKeyId
  , genSymmetricKey
  ) where

import           Data.ByteString.Internal (toForeignPtr, memcpy)

import           Foreign.C.Types (CUChar)
import           Foreign.Ptr (Ptr, plusPtr, castPtr)
import           Foreign.ForeignPtr (withForeignPtr)

import           P

import           System.IO (IO)

import           Tinfoil.Data.Key (SymmetricKey(..))

import           Zodiac.TSRP.Data.Key (KeyId(..))
import qualified Zodiac.TSRP.Key as Z

-- | Needs a 16B buffer.
genKeyId :: Ptr CUChar -> IO ()
genKeyId buf = do
  bs <- unKeyId <$> Z.genKeyId
  let (fp, offs, len) = toForeignPtr bs
  withForeignPtr fp $ \p ->
    memcpy (castPtr buf) (plusPtr p offs) len

-- | Needs a 32B buffer.
genSymmetricKey :: Ptr CUChar -> IO ()
genSymmetricKey buf = do
  bs <- unSymmetricKey <$> Z.genSymmetricKey
  let (fp, offs, len) = toForeignPtr bs
  withForeignPtr fp $ \p ->
    memcpy (castPtr buf) (plusPtr p offs) len

