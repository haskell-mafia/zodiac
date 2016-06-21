{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

{-
This module is the primary interface for signing and verifying TSRP
requests with zodiac and should provide everything needed to do so.
-}
module Zodiac.Core.TSRP (
    KeyId
  , SymmetricKey

  -- * Key generation
  , genKeyId
  , genSymmetricKey

  -- * Timestamps
  , timestampRequest

  -- * http-client interface
  , authedHttpClientRequest
  , httpClientKeyId
  , verifyHttpClientRequest
  , verifyHttpClientRequest'
  ) where

import           Tinfoil.Data.Key (SymmetricKey)

import           Zodiac.Core.Data.Key
import           Zodiac.Core.Key
import           Zodiac.Core.Time
import           Zodiac.Core.TSRP.HttpClient
