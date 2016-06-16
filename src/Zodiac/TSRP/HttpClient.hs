{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Zodiac.TSRP.HttpClient(
    authedHttpClientRequest
  , macHttpClientRequest
  , httpAuthHeader
  ) where

import qualified Data.CaseInsensitive as CI

import           Network.HTTP.Client (Request(..), requestHeaders)
import           Network.HTTP.Types (Header)

import           P

import           Tinfoil.Data (MAC, SymmetricKey)

import           Zodiac.Data
import           Zodiac.Request
import           Zodiac.Request.HttpClient
import           Zodiac.Symmetric

-- | Authenticate an http-client request. If the request isn't
-- malformed, the output is a Request object with the necessary
-- Authorization header added which can be sent directly to a server
-- supporting TSRP.
authedHttpClientRequest :: SymmetricProtocol
                        -> KeyId
                        -> SymmetricKey
                        -> RequestExpiry
                        -> Request
                        -> RequestTimestamp
                        -> Either RequestError Request
authedHttpClientRequest TSRPv1 kid sk re r rt =
  toCanonicalRequest r >>= \cr ->
    let mac = macRequest TSRPv1 kid rt re cr sk
        authH = httpAuthHeader TSRPv1 kid rt re cr mac
        newHeaders = authH : (requestHeaders r) in
    Right $ r { requestHeaders = newHeaders }

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
  (CI.mk "authorization", renderSymmetricAuthHeader sah)

-- | Create a detached MAC of an http-client request. This MAC can be
-- converted to an http-client-compatible Authorization header using
-- 'httpAuthHeader'.
macHttpClientRequest :: SymmetricProtocol
                     -> KeyId
                     -> SymmetricKey
                     -> RequestExpiry
                     -> Request
                     -> RequestTimestamp
                     -> Either RequestError MAC
macHttpClientRequest TSRPv1 kid sk re r rts = do
  cr <- toCanonicalRequest r
  pure $ macRequest TSRPv1 kid rts re cr sk
  


