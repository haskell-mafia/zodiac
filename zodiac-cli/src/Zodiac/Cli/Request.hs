{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Zodiac.Cli.Request(
    readHTTPRequest
  ) where

import           Control.Monad.IO.Class (liftIO)

import qualified Data.ByteString as BS

import           Hadron (HTTPRequest)

import           P

import           System.IO (IO)

import           X.Control.Monad.Trans.Either (EitherT, hoistEither)

import           Zodiac.Raw
import           Zodiac.Raw.Request

readHTTPRequest :: EitherT RequestError IO HTTPRequest
readHTTPRequest =
  hoistEither . parseRawRequest =<< liftIO BS.getContents
