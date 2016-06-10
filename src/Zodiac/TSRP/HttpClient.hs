{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Zodiac.TSRP.HttpClient(
    macHttpClientRequest
  ) where

import           Network.HTTP.Client (Request)

import           P

import           Tinfoil.Data (HashFunction(..), MAC, SymmetricKey)

import           Zodiac.Data
import           Zodiac.Request.HttpClient
import           Zodiac.Symmetric

macHttpClientRequest :: KeyId
                     -> SymmetricKey
                     -> RequestExpiry
                     -> Request
                     -> RequestTimestamp
                     -> Either RequestError MAC
macHttpClientRequest kid sk re r rts = do
  cr <- toCanonicalRequest r
  pure $ macRequest TSRPv1 SHA256 kid rts re cr sk
  


