{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Zodiac.Cli.TSRP.Parser(
    tsrpCommandP
  ) where

import qualified Data.ByteString.Char8 as BSC

import           Options.Applicative

import           P

import           X.Options.Applicative

import           Zodiac.Cli.Parser
import           Zodiac.Cli.TSRP.Data
import           Zodiac.Raw

tsrpCommandP :: Parser TSRPCommand
tsrpCommandP = subparser $
     command' "authenticate" "Authenticate an HTTP request read from standard input and write the authenticated request to standard output." authP
  <> command' "verify" "Verify the authentication of an HTTP request read from standard input." verifyP
  <> command' "debug" "Access debugging information and intermediate results." debugCommandP

authP :: Parser TSRPCommand
authP = TSRPAuth <$> lineEndingsP <*> requestExpiryP

verifyP :: Parser TSRPCommand
verifyP = TSRPVerify <$> lineEndingsP

debugCommandP :: Parser TSRPCommand
debugCommandP = subparser $
     command' "auth-string" "Derive string to authenticate and output it without computing the MAC." authStringP
  <> command' "canonise" "Read an HTTP request from standard input and write its canonical representation to stdout." canoniseP

authStringP :: Parser TSRPCommand
authStringP = TSRPDebugAuthString <$> lineEndingsP <*> requestExpiryP

canoniseP :: Parser TSRPCommand
canoniseP = TSRPDebugCanonise <$> lineEndingsP

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
