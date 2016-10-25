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

import           Zodiac.Cli.Process (preprocess)
import           Zodiac.Cli.TSRP.Data
import           Zodiac.Cli.TSRP.Env
import           Zodiac.Cli.TSRP.Error
import           Zodiac.Raw

-- | Read an HTTP request to authenticate from stdin and write the
-- authenticated request to stdout. Key ID and secret key are read
-- from the environment.
authenticate :: LineEndings -> RequestExpiry -> EitherT TSRPError IO ByteString
authenticate le re = do
  (TSRPParams sk kid) <- tsrpParamsFromEnv
  rt <- liftIO timestampRequest
  bs <- liftIO . fmap (preprocess le) $ BS.getContents
  firstEitherT TSRPRequestError . hoistEither $ authedRawRequest kid sk re bs rt

-- | Read an authenticated HTTP request on stdin and verify it with
-- the key ID and secret key provided in the environment.
verify :: LineEndings -> EitherT TSRPError IO ()
verify le = do
  (TSRPParams sk kid) <- tsrpParamsFromEnv
  bs <- liftIO . fmap (preprocess le) $ BS.getContents
  liftIO (verifyRawRequest kid sk bs) >>= \case
    Verified -> pure ()
    NotVerified -> left TSRPRequestNotVerifiedError
    VerificationError -> left TSRPVerificationError
