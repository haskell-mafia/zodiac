{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-
Request timing functionality.
-}
module Zodiac.Core.Time(
    expiresAt
  , requestExpired
  , timestampRequest
  ) where

import           Data.Time.Clock (UTCTime(..), addUTCTime, getCurrentTime)
import           Data.Time.Clock (NominalDiffTime, diffUTCTime)

import           P

import           System.IO (IO)

import           Zodiac.Core.Data.Time

-- | Returns the current time as a request timestamp.
timestampRequest :: IO RequestTimestamp
timestampRequest = RequestTimestamp <$> getCurrentTime

requestExpired :: RequestTimestamp -> RequestExpiry -> UTCTime -> RequestExpired
requestExpired rt re now
  | clockSkew rt now > maxClockSkew = NotYetValid
  | (expiresAt rt re) <= now        = TimeExpired
  | otherwise                       = TimeValid

-- | How much greater the request time is than the current time. This
-- will generally be negative unless the the server's clock or the
-- client's clock is skewed (or the request is malicious).
clockSkew :: RequestTimestamp -> UTCTime -> NominalDiffTime
clockSkew (RequestTimestamp rt) now = diffUTCTime rt now

expiresAt :: RequestTimestamp -> RequestExpiry -> UTCTime
expiresAt (RequestTimestamp rt) (RequestExpiry re) =
  addUTCTime (fromIntegral re) rt
