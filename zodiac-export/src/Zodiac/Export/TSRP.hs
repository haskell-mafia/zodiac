{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Zodiac.Export.TSRP (
    verifyRawRequest'
  ) where

import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as T

import           Foreign.C.Types (CChar, CInt, CSize, CTime)
import           Foreign.Ptr (Ptr)

import           P

import           System.IO (IO)

import qualified Zodiac.Core.Data as Z
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
  -- FIXME: too many magic numbers
  bsKid <- BS.packCStringLen (bufKid, 16)
  bsSK <- BS.packCStringLen (bufSK, 32)
  bsReq <- BS.packCStringLen (bufReq, fromIntegral sizeReq)
  let parsed = do
                 tsk <- either (const Nothing') Just' $ T.decodeUtf8' bsSK
                 kid <- Z.parseKeyId bsKid
                 sk <- Z.parseSymmetricKey tsk
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


