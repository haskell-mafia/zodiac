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

import           Tinfoil.Data (HashFunction(..), MAC, SymmetricKey)

import           Zodiac.Data
import           Zodiac.Request
import           Zodiac.Request.HttpClient
import           Zodiac.Symmetric

authedHttpClientRequest :: SymmetricProtocol
                        -> HashFunction
                        -> KeyId
                        -> SymmetricKey
                        -> RequestExpiry
                        -> Request
                        -> RequestTimestamp
                        -> Either RequestError Request
authedHttpClientRequest TSRPv1 hf kid sk re r rt =
  toCanonicalRequest r >>= \cr ->
    let mac = macRequest TSRPv1 hf kid rt re cr sk
        authH = httpAuthHeader TSRPv1 hf kid rt re cr mac
        newHeaders = authH : (requestHeaders r) in
    Right $ r { requestHeaders = newHeaders }

httpAuthHeader :: SymmetricProtocol
               -> HashFunction
               -> KeyId
               -> RequestTimestamp
               -> RequestExpiry
               -> CRequest
               -> MAC
               -> Header
httpAuthHeader TSRPv1 hf kid rt re cr mac =
  let sh = signedHeaders cr
      sah = SymmetricAuthHeader TSRPv1 hf kid rt re sh mac in
  (CI.mk "authorization", renderSymmetricAuthHeader sah)

macHttpClientRequest :: SymmetricProtocol
                     -> HashFunction
                     -> KeyId
                     -> SymmetricKey
                     -> RequestExpiry
                     -> Request
                     -> RequestTimestamp
                     -> Either RequestError MAC
macHttpClientRequest TSRPv1 hf kid sk re r rts = do
  cr <- toCanonicalRequest r
  pure $ macRequest TSRPv1 hf kid rts re cr sk
  


