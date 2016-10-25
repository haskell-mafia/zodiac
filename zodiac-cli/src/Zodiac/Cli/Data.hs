{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Zodiac.Cli.Data(
    LineEndings(..)
  ) where

import           P

-- | Line endings in the input. If they're UNIX, we need to convert them before
-- they can be parsed as an HTTP request.
data LineEndings =
    LF
  | CRLF
  deriving (Eq, Show)
