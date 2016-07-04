{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Zodiac.Cli.TSRP.Commands(
    authenticate
  , verify
  ) where

import           Data.ByteString (ByteString)

import           System.IO (IO)

import           X.Control.Monad.Trans.Either (EitherT, left)

import           Zodiac.Cli.TSRP.Error
import           Zodiac.Raw

-- | Read an HTTP request to authenticate from stdin and write the
-- authenticated request to stdout. Key ID and secret key are read
-- from the environment.
authenticate :: RequestExpiry -> EitherT TSRPError IO ByteString
authenticate _re = left TSRPNotImplementedError

-- | Read an authenticated HTTP request on stdin and verify it with
-- the key ID and secret key provided in the environment.
verify :: EitherT TSRPError IO ()
verify = left TSRPNotImplementedError
