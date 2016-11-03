{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Zodiac.Cli.ReqTool.Commands(
    canonise
  ) where

import           Control.Monad.IO.Class (liftIO)

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import           P

import           System.IO (IO)

import           X.Control.Monad.Trans.Either (EitherT, firstEitherT, hoistEither)

import           Zodiac.Cli.Data (LineEndings(..))
import           Zodiac.Cli.Process (preprocess)
import           Zodiac.Cli.ReqTool.Error
import           Zodiac.Core.Request (renderCRequest)
import           Zodiac.Raw.Request (toCanonicalRequest)

-- | Read an HTTP request to authenticate from stdin and write the
-- canonical form to stdout.
canonise :: LineEndings -> EitherT ReqToolError IO ByteString
canonise le = do
  bs <- liftIO . fmap (preprocess le) $ BS.getContents
  cr <- firstEitherT ReqToolRequestError . hoistEither $ toCanonicalRequest bs
  -- Not supposed to be a text document, doesn't have a newline terminator.
  pure $ renderCRequest cr
