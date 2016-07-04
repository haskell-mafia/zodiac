{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Zodiac.Cli.TSRP.Parser(
    tsrpCommandP
  ) where

import           Options.Applicative (Parser)

import           P

import           X.Options.Applicative (subparser, command')

import           Zodiac.Cli.TSRP.Data

tsrpCommandP :: Parser TSRPCommand
tsrpCommandP = subparser $
     command' "validate" "Verify that an HTTP request read from standard input is a valid request for TSRP authentication." validateP

validateP :: Parser TSRPCommand
validateP = pure TSRPValidate
