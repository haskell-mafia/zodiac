{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Zodiac.Export.TSRP (
    verifyRawRequest'
  ) where

import qualified Data.ByteString as BS

import           Foreign.C.Types (CChar, CInt, CSize, CTime)
import           Foreign.Ptr (Ptr)

import           P

import           System.IO (IO)

import qualified Zodiac.TSRP.Data as Z
import           Zodiac.Raw (Verified(..))
import qualified Zodiac.Raw as Z
import           Zodiac.Export.Time

-- FIXME: set errno
verifyRawRequest' :: Ptr CChar -- ^ Key ID, 16 bytes.
                  -> Ptr CChar -- ^ Secret key, 32 bytes.
                  -> Ptr CChar -- ^ HTTP request, variable-length.
                  -> CSize -- ^ Size of HTTP request buffer.
                  -> CTime -- ^ Current time.
                  -> IO CInt
verifyRawRequest' bufKid bufSK bufReq sizeReq ts =
  let vt = Z.unRequestTimestamp $ cTimeToTimestamp ts in do
  bsKid <- BS.packCStringLen (bufKid, Z.tsrpKeyIdLength)
  bsSK <- BS.packCStringLen (bufSK, Z.tsrpSecretKeyLength)
  bsReq <- BS.packCStringLen (bufReq, fromIntegral sizeReq)
  let parsed = do
                 kid <- Z.parseKeyId bsKid
                 sk <- Z.parseTSRPKey bsSK
                 pure (kid, sk)
  case parsed of
    Nothing' ->
      -- FIXME: errno
      pure (-1)
    Just' (kid, sk) ->
      Z.verifyRawRequest' kid sk bsReq vt >>= \case
        Verified -> pure 0
        NotVerified -> pure 1
        VerificationError -> pure (-1)


