{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Zodiac.Cli.TSRP.Data(
    TSRPCommand(..)
  , TSRPParams(..)
  , LineEndings(..)
  ) where

import           P

import           Zodiac.Cli.Data (LineEndings(..))
import           Zodiac.Raw

data TSRPCommand =
    TSRPAuth !LineEndings !RequestExpiry
  | TSRPVerify !LineEndings
  | TSRPDebugAuthString !LineEndings !RequestExpiry
  | TSRPDebugCanonise !LineEndings
  deriving (Eq, Show)

-- | Parameters required for auth/verification.
--
-- These are things which would normally be provided via environment
-- variables on *nix, but due to potential need to support Windows we're
-- keeping this abstract interface.
data TSRPParams = TSRPParams {
    tsrpSecretKey :: !TSRPKey
  , tsrpKeyId :: !KeyId
  } deriving (Eq, Show)
