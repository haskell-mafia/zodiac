{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

{-
This module is the primary interface for signing and verifying http-client 
TSRP requests with zodiac and should provide everything needed to do so.
-}
module Zodiac.HttpClient(
    KeyId
  , TSRPKey
  , RequestTimestamp(..)
  , SymmetricProtocol(..)
  , Verified(..)

  , parseTSRPKey
  , parseSymmetricProtocol
  , parseKeyId
  , renderKeyId
  , renderTSRPKey
  , renderSymmetricProtocol

  -- * Key generation
  , genKeyId
  , genTSRPKey

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

import           Tinfoil.Data (Verified(..))

import           Zodiac.Core.Data.Protocol
import           Zodiac.Core.Data.Time
import           Zodiac.Core.Time
import           Zodiac.HttpClient.Error
import           Zodiac.HttpClient.TSRP
import           Zodiac.TSRP.Data.Key
import           Zodiac.TSRP.Key
