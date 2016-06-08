{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Zodiac.Symmetric(
    authenticationString
  , macRequest
  ) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as T

import           P

import           Tinfoil.Data (HashFunction, renderHashFunction, renderHash)
import           Tinfoil.Data (MAC, SymmetricKey, keyHashFunction)
import           Tinfoil.Hash (hash)
import           Tinfoil.MAC (macBytes)

import           Zodiac.Data
import           Zodiac.MAC
import           Zodiac.Request

-- | Authenticate a 'CanonicalRequest' with a secret key.
macRequest :: SymmetricProtocol
           -> HashFunction
           -> KeyId
           -> RequestTimestamp
           -> RequestExpiry
           -> CRequest
           -> SymmetricKey
           -> MAC
macRequest TSRPv1 hf kid rts re cr sk =
  let authKey = deriveRequestKey TSRPv1 (timestampDate rts) kid sk
      authString = authenticationString TSRPv1 hf kid rts re cr
      khf = keyHashFunction hf in
  macBytes khf authKey authString

-- | Construct a string to authenticate, from all the information which will
-- be included in the request MAC.
authenticationString :: SymmetricProtocol
                     -> HashFunction
                     -> KeyId
                     -> RequestTimestamp
                     -> RequestExpiry
                     -> CRequest
                     -> ByteString
authenticationString TSRPv1 hf kid rts re cr =
  let hcr = hash hf $ renderCRequest cr in
  BS.intercalate "\n" [
      renderSymmetricProtocol TSRPv1
    , T.encodeUtf8 (renderHashFunction hf)
    , renderKeyId kid
    , renderRequestTimestamp rts
    , renderRequestExpiry re
    , T.encodeUtf8 (renderHash hcr)
    ]
