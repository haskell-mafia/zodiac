{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Zodiac.HttpClient.TSRP(
    authedHttpClientRequest
  , macHttpClientRequest
  , httpAuthHeader
  , httpClientKeyId
  , httpClientAuthHeader
  , verifyHttpClientRequest
  , verifyHttpClientRequest'
  ) where

import           Data.Time.Clock (UTCTime, getCurrentTime)

import           Network.HTTP.Client (Request(..), requestHeaders)
import           Network.HTTP.Types (Header)
import           Network.HTTP.Types.Header (hAuthorization)

import           P

import           System.IO (IO)

import           Tinfoil.Data (Verified(..), MAC)

import           Zodiac.Core.Request
import           Zodiac.TSRP.Data
import           Zodiac.TSRP.Symmetric
import           Zodiac.HttpClient.Error
import           Zodiac.HttpClient.Request

-- | Authenticate an http-client request. If the request isn't
-- malformed, the output is a Request object with the necessary
-- Authorization header added which can be sent directly to a server
-- supporting TSRP.
authedHttpClientRequest :: KeyId
                        -> TSRPKey
                        -> RequestExpiry
                        -> Request
                        -> RequestTimestamp
                        -> Either RequestError Request
authedHttpClientRequest kid sk re r rt =
  toCanonicalRequest r >>= \cr ->
    let mac = macRequest TSRPv1 kid rt re cr sk
        authH = httpAuthHeader TSRPv1 kid rt re cr mac
        newHeaders = authH : (requestHeaders r) in
    Right $ r { requestHeaders = newHeaders }

-- | Works as 'verifyHttpClientRequest'', but uses the current time to verify
-- the request.
verifyHttpClientRequest :: KeyId
                        -> TSRPKey
                        -> Request
                        -> IO Verified
verifyHttpClientRequest kid sk r =
  getCurrentTime >>= (verifyHttpClientRequest' kid sk r)

-- | Verify an authenticated http-client request. The 'KeyId' parameter
-- can be extracted with 'httpClientKeyId'; the 'TSRPKey' should be
-- the one associated with the 'KeyId'.
verifyHttpClientRequest' :: KeyId
                         -> TSRPKey
                         -> Request
                         -> UTCTime
                         -> IO Verified
verifyHttpClientRequest' kid sk req now =
  case httpClientAuthHeader req of
    Left _ ->
      pure NotVerified
    Right sah ->
      case toCanonicalRequest req of
        Left _ -> pure NotVerified
        Right cr -> verifyRequest kid sk cr sah now

-- | Extract the 'KeyId' from a request.
httpClientKeyId :: Request
                -> Either ProtocolError KeyId
httpClientKeyId r =
  httpClientAuthHeader r >>= (pure . sahKeyId)

httpClientAuthHeader :: Request
                     -> Either ProtocolError SymmetricAuthHeader
httpClientAuthHeader r =
  let hs = requestHeaders r in
  case filter ((== hAuthorization) . fst) hs of
    [] ->
      Left NoAuthHeader
    [(_, v)] ->
      maybe' (Left MalformedAuthHeader) Right $ parseSymmetricAuthHeader v
    _ -> Left MultipleAuthHeaders

-- | Given a precomputed MAC of a request, construct the appropriate
-- Authorization header in a form suitable to be used with
-- http-client.
httpAuthHeader :: SymmetricProtocol
               -> KeyId
               -> RequestTimestamp
               -> RequestExpiry
               -> CRequest
               -> MAC
               -> Header
httpAuthHeader TSRPv1 kid rt re cr mac =
  let sh = signedHeaders cr
      sah = SymmetricAuthHeader TSRPv1 kid rt re sh mac in
  (hAuthorization, renderSymmetricAuthHeader sah)

-- | Create a detached MAC of an http-client request. This MAC can be
-- converted to an http-client-compatible Authorization header using
-- 'httpAuthHeader'.
macHttpClientRequest :: KeyId
                     -> TSRPKey
                     -> RequestExpiry
                     -> Request
                     -> RequestTimestamp
                     -> Either RequestError MAC
macHttpClientRequest kid sk re r rts = do
  cr <- toCanonicalRequest r
  pure $ macRequest TSRPv1 kid rts re cr sk
  


