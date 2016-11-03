{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Zodiac.Cli.ReqTool.Data(
    ReqToolCommand(..)
  ) where

import           P

import           Zodiac.Cli.Data (LineEndings(..))

data ReqToolCommand =
    ReqToolCanonise !LineEndings
  deriving (Eq, Show)
