{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Zodiac.Export.Time (
    timestampToCTime
  , cTimeToTimestamp
  ) where

import           Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds, posixSecondsToUTCTime)

import           Foreign.C.Types (CTime(..))

import           P

import           Zodiac.Core.Data.Time (RequestTimestamp(..))

timestampToCTime :: RequestTimestamp -> CTime
timestampToCTime =
  CTime . floor . utcTimeToPOSIXSeconds . unRequestTimestamp

cTimeToTimestamp :: CTime -> RequestTimestamp
cTimeToTimestamp =
  RequestTimestamp . posixSecondsToUTCTime . realToFrac
