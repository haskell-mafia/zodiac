{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Zodiac.Symmetric(
    authenticationString
  , macRequest
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

-- | Verify a request MAC.
verifyRequest' :: SymmetricProtocol
               -> KeyId
               -> RequestTimestamp
               -> RequestExpiry
               -> CRequest
               -> SymmetricKey
               -> MAC
               -> UTCTime
               -> IO Verified
verifyRequest' TSRPv1 kid rts re cr sk mac now =
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
