{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

{-
This module is the primary interface for signing and verifying http-client 
TSRP requests with zodiac and should provide everything needed to do so.
-}
module Zodiac.HttpClient(
    KeyId
  , SymmetricKey
  , SymmetricProtocol(..)

  , parseSymmetricKey
  , parseSymmetricProtocol
  , parseKeyId
  , renderKeyId
  , renderSymmetricKey
  , renderSymmetricProtocol

  -- * Key generation
  , genKeyId
  , genSymmetricKey

  -- * Timestamps
  , RequestExpiry(..)
  , parseRequestExpiry
  , renderRequestExpiry
  , timestampRequest

  -- * http-client interface
  , authedHttpClientRequest
  , httpClientKeyId
  , verifyHttpClientRequest
  , verifyHttpClientRequest'
  , RequestError(..)
  , renderRequestError
  ) where

import           Tinfoil.Data.Key (SymmetricKey, parseSymmetricKey, renderSymmetricKey)

import           Zodiac.Core.Data.Key
import           Zodiac.Core.Data.Protocol
import           Zodiac.Core.Data.Time
import           Zodiac.Core.Key
import           Zodiac.Core.Time
import           Zodiac.HttpClient.Error
import           Zodiac.HttpClient.TSRP
