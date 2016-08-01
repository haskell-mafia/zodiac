{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Zodiac.Export.Key (
    genKeyId
  , genTSRPKey
  ) where

import           Data.ByteString.Internal (toForeignPtr, memcpy)

import           Foreign.C.Types (CUChar)
import           Foreign.Ptr (Ptr, plusPtr, castPtr)
import           Foreign.ForeignPtr (withForeignPtr)

import           P

import           System.IO (IO)

import           Tinfoil.Data.Key (SymmetricKey(..))

import           Zodiac.TSRP.Data.Key (KeyId(..), TSRPKey(..))
import qualified Zodiac.TSRP.Key as Z

-- | Needs a 16B buffer.
genKeyId :: Ptr CUChar -> IO ()
genKeyId buf = do
  bs <- unKeyId <$> Z.genKeyId
  let (fp, offs, len) = toForeignPtr bs
  withForeignPtr fp $ \p ->
    memcpy (castPtr buf) (plusPtr p offs) len

-- | Needs a 32B buffer.
genTSRPKey :: Ptr CUChar -> IO ()
genTSRPKey buf = do
  bs <- (unSymmetricKey . unTSRPKey) <$> Z.genTSRPKey
  let (fp, offs, len) = toForeignPtr bs
  withForeignPtr fp $ \p ->
    memcpy (castPtr buf) (plusPtr p offs) len

