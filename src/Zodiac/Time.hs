{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-
Request timing functionality.
-}
module Zodiac.Time(
    expiresAt
  , requestExpired
  ) where

import           Data.Time.Clock (UTCTime(..), addUTCTime)

import           P

import           Zodiac.Data.Time

requestExpired :: RequestTimestamp -> RequestExpiry -> UTCTime -> RequestExpired
requestExpired rt re now
  | (unRequestTimestamp rt) > now = NotYetValid
  | (expiresAt rt re) <= now      = TimeExpired
  | otherwise                     = TimeValid

expiresAt :: RequestTimestamp -> RequestExpiry -> UTCTime
expiresAt (RequestTimestamp rt) (RequestExpiry re) =
  addUTCTime (fromIntegral re) rt
