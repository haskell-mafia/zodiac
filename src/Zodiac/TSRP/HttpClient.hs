{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Zodiac.TSRP.HttpClient(
    authedHttpClientRequest
  , macHttpClientRequest
  , httpAuthHeader
  , httpClientKeyId
  , httpClientAuthHeader
  , verifyHttpClientRequest'
  ) where

import qualified Data.CaseInsensitive as CI
import           Data.Time.Clock (UTCTime)
import qualified Data.Text.Encoding as T

import           Network.HTTP.Client (Request(..), requestHeaders)
import           Network.HTTP.Types (Header)

import           P

import           System.IO (IO)

import           Tinfoil.Data (Verified(..), MAC, SymmetricKey)

import           Zodiac.Data
import           Zodiac.Request
import           Zodiac.Request.HttpClient
import           Zodiac.Symmetric

-- | Authenticate an http-client request. If the request isn't
-- malformed, the output is a Request object with the necessary
-- Authorization header added which can be sent directly to a server
-- supporting TSRP.
authedHttpClientRequest :: KeyId
                        -> SymmetricKey
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

verifyHttpClientRequest' :: UTCTime
                         -> KeyId
                         -> SymmetricKey
                         -> Request
                         -> IO Verified
verifyHttpClientRequest' now kid sk req =
  case httpClientAuthHeader req of
    Left _ ->
      pure NotVerified
    Right (SymmetricAuthHeader sp kid' rt re csh mac) ->
      if kid /= kid'
        then pure NotVerified
        else case toCanonicalRequest req of
          Left _ -> pure NotVerified
          Right cr ->
            let cr' = stripUnsignedHeaders cr csh in
            verifyRequest' sp kid rt re cr' sk mac now

-- | Extract the 'KeyId' from a request.
httpClientKeyId :: Request
                -> Either ProtocolError KeyId
httpClientKeyId r =
  httpClientAuthHeader r >>= (pure . sahKeyId)

httpClientAuthHeader :: Request
                     -> Either ProtocolError SymmetricAuthHeader
httpClientAuthHeader r =
  let hs = requestHeaders r in
  case filter ((== auth) . fst) hs of
    [] ->
      Left NoAuthHeader
    [(_, v)] ->
      maybe' (Left MalformedAuthHeader) Right $ parseSymmetricAuthHeader v
    _ -> Left MultipleAuthHeaders
  where
    auth = CI.mk $ T.encodeUtf8 authHeaderName

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
  (CI.mk $ T.encodeUtf8 authHeaderName, renderSymmetricAuthHeader sah)

-- | Create a detached MAC of an http-client request. This MAC can be
-- converted to an http-client-compatible Authorization header using
-- 'httpAuthHeader'.
macHttpClientRequest :: KeyId
                     -> SymmetricKey
                     -> RequestExpiry
                     -> Request
                     -> RequestTimestamp
                     -> Either RequestError MAC
macHttpClientRequest kid sk re r rts = do
  cr <- toCanonicalRequest r
  pure $ macRequest TSRPv1 kid rts re cr sk
  


