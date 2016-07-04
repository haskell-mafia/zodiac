{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Zodiac.Cli.TSRP.Parser(
    tsrpCommandP
  ) where

import qualified Data.ByteString.Char8 as BSC

import           Options.Applicative

import           P

import           X.Options.Applicative

import           Zodiac.Cli.TSRP.Data
import           Zodiac.Raw

tsrpCommandP :: Parser TSRPCommand
tsrpCommandP = subparser $
     command' "validate" "Verify that an HTTP request read from standard input is a valid request for TSRP authentication." validateP
  <> command' "authenticate" "Authenticate an HTTP request read from standard input and write the authenticated request to standard output." authP
  <> command' "verify" "Verify the authentication of an HTTP request read from standard input." verifyP

validateP :: Parser TSRPCommand
validateP = pure TSRPValidate

authP :: Parser TSRPCommand
authP = TSRPAuth <$> requestExpiryP

verifyP :: Parser TSRPCommand
verifyP = pure TSRPVerify

requestExpiryP :: Parser RequestExpiry
requestExpiryP = option (eitherReader requestExpiryR) $
     metavar "EXPIRY"
  <> short 'e'
  <> long "request-expiry"
  <> value (RequestExpiry 60)
  <> help "Request expiry time in seconds (after the current time). Default is 60."
  where
    requestExpiryR x = case parseRequestExpiry (BSC.pack x) of
      Nothing' -> Left $ "invalid request expiry: " <> x
      Just' re -> pure re
