{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Zodiac.Cli.TSRP.Commands(
    validate
  ) where

import           P

import           System.IO (IO)

import           X.Control.Monad.Trans.Either (EitherT, firstEitherT)

import           Zodiac.Cli.Request
import           Zodiac.Cli.TSRP.Error

-- | Read in an HTTP request from stdin and check whether it's valid.
--
-- FIXME: validate TSRP-specific bits like request method as well.
validate :: EitherT TSRPError IO ()
validate = void $ firstEitherT TSRPRequestError readHTTPRequest
