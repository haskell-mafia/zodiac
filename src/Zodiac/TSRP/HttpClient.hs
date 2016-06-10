{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Zodiac.TSRP.HttpClient(
    macHttpClientRequest
  , httpAuthHeader
  ) where

import qualified Data.CaseInsensitive as CI

import           Network.HTTP.Client (Request)
import           Network.HTTP.Types (Header)

import           P

import           Tinfoil.Data (HashFunction(..), MAC, SymmetricKey)

import           Zodiac.Data
import           Zodiac.Request
import           Zodiac.Request.HttpClient
import           Zodiac.Symmetric

httpAuthHeader :: SymmetricProtocol
               -> HashFunction
               -> KeyId
               -> RequestTimestamp
               -> RequestExpiry
               -> CRequest
               -> MAC
               -> Header
httpAuthHeader sp hf kid rt re cr mac =
  let sh = signedHeaders cr
      sah = SymmetricAuthHeader sp hf kid rt re sh mac in
  (CI.mk "Authorization", renderSymmetricAuthHeader sah)

macHttpClientRequest :: SymmetricProtocol
                     -> HashFunction
                     -> KeyId
                     -> SymmetricKey
                     -> RequestExpiry
                     -> Request
                     -> RequestTimestamp
                     -> Either RequestError MAC
macHttpClientRequest sp hf kid sk re r rts = do
  cr <- toCanonicalRequest r
  pure $ macRequest sp hf kid rts re cr sk
  


