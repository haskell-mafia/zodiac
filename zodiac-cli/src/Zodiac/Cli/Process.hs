{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Zodiac.Cli.Process(
    preprocess
  ) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import           P

import           Zodiac.Cli.Data

-- | Convert line endings if we need to.
preprocess :: LineEndings -> ByteString -> ByteString
preprocess CRLF =
  id
preprocess LF =
  -- Split on \n, unite with \r\n.
  BS.intercalate crlf . BS.split 0x0a

crlf :: ByteString
crlf = BS.pack [0x0d, 0x0a]
