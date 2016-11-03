{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Zodiac.Cli.ReqTool.Parser(
    reqToolCommandP
  ) where

import           Options.Applicative

import           P

import           X.Options.Applicative

import           Zodiac.Cli.Parser
import           Zodiac.Cli.ReqTool.Data

reqToolCommandP :: Parser ReqToolCommand
reqToolCommandP = subparser $
     command' "canonise" "Read an HTTP request from standard input and write its canonical representation to stdout." canoniseP

canoniseP :: Parser ReqToolCommand
canoniseP = ReqToolCanonise <$> lineEndingsP
