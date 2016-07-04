{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Zodiac.Cli.TSRP.Data(
    TSRPCommand(..)
  , TSRPParams(..)
  ) where

import           P

import           Zodiac.Raw

data TSRPCommand =
    TSRPAuth !RequestExpiry
  | TSRPVerify
  | TSRPValidate
  deriving (Eq, Show)

-- | Parameters required for auth/verification.
--
-- These are things which would normally be provided via environment
-- variables on *nix, but due to potential need to support Windows we're
-- keeping this abstract interface.
data TSRPParams = TSRPParams {
    tsrpSecretKey :: !SymmetricKey
  , tsrpKeyId :: !KeyId
  } deriving (Eq)

instance Show TSRPParams where
  showsPrec p (TSRPParams _sk kid) =
    showParen (p > 10) $
      showString "TSRPParams (SymmetricKey \"<redacted>\") " .
      showsPrec 11 kid
