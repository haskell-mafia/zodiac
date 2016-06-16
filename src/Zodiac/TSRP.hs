{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Zodiac.TSRP (
    KeyId
  , SymmetricKey

  , authedHttpClientRequest
  , genKeyId
  , genSymmetricKey
  , timestampRequest
  ) where

import           Tinfoil.Data.Key (SymmetricKey)

import           Zodiac.Data.Key
import           Zodiac.Key
import           Zodiac.Time
import           Zodiac.TSRP.HttpClient
