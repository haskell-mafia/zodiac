{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

{-
This module is the primary interface for signing and verifying raw HTTP
TSRP requests with zodiac and should provide everything needed to do so.
-}
module Zodiac.Raw(
    KeyId
  , SymmetricKey
  , SymmetricProtocol(..)

  , parseKeyId
  , parseSymmetricKey
  , parseSymmetricProtocol
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

  -- * raw HTTP interface
  , authedRawRequest
  , rawKeyId
  , verifyRawRequest
  , verifyRawRequest'
  , RequestError(..)
  , renderRequestError
  ) where

import           Tinfoil.Data.Key (SymmetricKey, parseSymmetricKey, renderSymmetricKey)

import           Zodiac.Core.Data.Key
import           Zodiac.Core.Data.Protocol
import           Zodiac.Core.Data.Time
import           Zodiac.Core.Key
import           Zodiac.Core.Time
import           Zodiac.Raw.Error
import           Zodiac.Raw.TSRP
