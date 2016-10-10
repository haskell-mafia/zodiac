{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

{-
This module is the primary interface for signing and verifying raw HTTP
TSRP requests with zodiac and should provide everything needed to do so.
-}
module Zodiac.Raw(
    KeyId
  , TSRPKey
  , RequestTimestamp(..)
  , SymmetricProtocol(..)
  , Verified(..)

  , parseKeyId
  , parseTSRPKey
  , parseSymmetricProtocol
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

  -- * raw HTTP interface
  , authedRawRequest
  , rawKeyId
  , verifyRawRequest
  , verifyRawRequest'
  , RequestError(..)
  , renderRequestError
  ) where

import           Tinfoil.Data (Verified(..))

import           Zodiac.Core.Data.Protocol
import           Zodiac.Core.Data.Time
import           Zodiac.Core.Time
import           Zodiac.Raw.Error
import           Zodiac.Raw.TSRP
import           Zodiac.TSRP.Data.Key
import           Zodiac.TSRP.Key
