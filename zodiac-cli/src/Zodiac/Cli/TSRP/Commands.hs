{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Zodiac.Cli.TSRP.Commands(
    authenticate
  , verify
  ) where

import           Control.Monad.IO.Class (liftIO)

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import           P

import           System.IO (IO)

import           X.Control.Monad.Trans.Either (EitherT, left, firstEitherT, hoistEither)

import           Zodiac.Cli.TSRP.Data
import           Zodiac.Cli.TSRP.Env
import           Zodiac.Cli.TSRP.Error
import           Zodiac.Raw

-- | Read an HTTP request to authenticate from stdin and write the
-- authenticated request to stdout. Key ID and secret key are read
-- from the environment.
authenticate :: RequestExpiry -> EitherT TSRPError IO ByteString
authenticate re = do
  (TSRPParams sk kid) <- tsrpParamsFromEnv
  rt <- liftIO timestampRequest
  bs <- liftIO BS.getContents
  firstEitherT TSRPRequestError . hoistEither $ authedRawRequest kid sk re bs rt

-- | Read an authenticated HTTP request on stdin and verify it with
-- the key ID and secret key provided in the environment.
verify :: EitherT TSRPError IO ()
verify = left TSRPNotImplementedError
