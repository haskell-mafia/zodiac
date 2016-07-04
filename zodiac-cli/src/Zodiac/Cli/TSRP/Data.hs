{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Zodiac.Cli.TSRP.Data(
    TSRPCommand(..)
  , TSRPParams(..)
  ) where

import           Data.ByteString (ByteString)

import           P

import           Zodiac.Raw

data TSRPCommand =
    TSRPAuth !TSRPParams !RequestExpiry !ByteString
  | TSRPVerify !TSRPParams !ByteString
  deriving (Eq)

-- | Parameters required for auth/verification.
--
-- These are things which would normally be provided via environment
-- variables on *nix, but due to potential need to support Windows we're
-- keeping this abstract interface.
data TSRPParams = TSRPParams {
    tsrpSecretKey :: !SymmetricKey
  , tsrpKeyId :: !KeyId
  } deriving (Eq)
