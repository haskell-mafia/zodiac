{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Zodiac.Symmetric(
    authenticationString
  , macRequest
  , verifyRequest
  , verifyRequest'
  ) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Time.Clock (UTCTime)
import qualified Data.Text.Encoding as T

import           P

import           System.IO (IO)

import           Tinfoil.Data (Verified(..), MAC, SymmetricKey)
import           Tinfoil.Data (KeyedHashFunction(..), HashFunction(..))
import           Tinfoil.Data (renderHash)
import           Tinfoil.Hash (hash)
import           Tinfoil.MAC (macBytes, verifyMAC)

import           Zodiac.Data
import           Zodiac.MAC
import           Zodiac.Request
import           Zodiac.Time

-- | Authenticate a 'CanonicalRequest' with a secret key.
macRequest :: SymmetricProtocol
           -> KeyId
           -> RequestTimestamp
           -> RequestExpiry
           -> CRequest
           -> SymmetricKey
           -> MAC
macRequest TSRPv1 kid rts re cr sk =
  let authKey = deriveRequestKey TSRPv1 (timestampDate rts) kid sk
      authString = authenticationString TSRPv1 kid rts re cr in
  macBytes HMAC_SHA256 authKey authString

-- | Verify a request and authentication header.
--
-- The provided 'KeyId' and 'SymmetricKey' should be those stored by the
-- server. The key ID will be checked against the one contained in the
-- 'SymmetricAuthHeader'.
verifyRequest :: KeyId
              -> SymmetricKey
              -> CRequest
              -> SymmetricAuthHeader
              -> UTCTime
              -> IO Verified
verifyRequest kid sk cr (SymmetricAuthHeader sp kid' rt re csh mac) now =
  if kid /= kid'
    then
      pure NotVerified
    else
      let cr' = stripCRequest cr csh in
      verifyRequest' sp kid rt re cr' sk mac now

-- | Verify the MAC of an unpacked request.
--
-- This is the lowest-level verification in zodiac.
--
-- This function:
--  * Derives the auth key.
--  * Derives the string to MAC.
--  * Verifies that the request is within its window of validity (based on the
--    provided "current" time).
--  * Computes the HMAC.
--  * Compares the computed value to the provided HMAC (in constant time).
--
-- This function does not:
--  * Verify that the key ID is actually associated with a requester.
--  * Strip unsigned headers from requests.
verifyRequest' :: SymmetricProtocol
               -> KeyId
               -> RequestTimestamp
               -> RequestExpiry
               -> StrippedCRequest
               -> SymmetricKey
               -> MAC
               -> UTCTime
               -> IO Verified
verifyRequest' TSRPv1 kid rts re (StrippedCRequest cr) sk mac now =
  let authKey = deriveRequestKey TSRPv1 (timestampDate rts) kid sk
      authString = authenticationString TSRPv1 kid rts re cr in
  case requestExpired rts re now of
    NotYetValid -> pure NotVerified
    TimeExpired -> pure NotVerified
    TimeValid -> verifyMAC HMAC_SHA256 authKey authString mac

-- | Construct a string to authenticate, from all the information which will
-- be included in the request MAC.
authenticationString :: SymmetricProtocol
                     -> KeyId
                     -> RequestTimestamp
                     -> RequestExpiry
                     -> CRequest
                     -> ByteString
authenticationString TSRPv1 kid rts re cr =
  let hcr = hash SHA256 $ renderCRequest cr in
  BS.intercalate "\n" [
      renderSymmetricProtocol TSRPv1
    , renderKeyId kid
    , renderRequestTimestamp rts
    , renderRequestExpiry re
    , T.encodeUtf8 (renderHash hcr)
    ]
