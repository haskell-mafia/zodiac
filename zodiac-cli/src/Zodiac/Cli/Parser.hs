{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Zodiac.Cli.Parser(
    lineEndingsP
  ) where

import           P

import           X.Options.Applicative

import           Zodiac.Cli.Data

lineEndingsP :: Parser LineEndings
lineEndingsP = flag CRLF LF $
     short 'u'
  <> long "unix-line-endings"
  <> help "Convert UNIX line-endings into CRLF in the provided HTTP request."
