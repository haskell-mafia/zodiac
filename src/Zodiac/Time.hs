{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-
Request timing functionality.
-}
module Zodiac.Time(
    expiresAt
  , requestExpired
  , timestampRequest
  ) where

import           Data.Time.Clock (UTCTime(..), addUTCTime, getCurrentTime)

import           P

import           System.IO (IO)

import           Zodiac.Data.Time

-- | Returns the current time as a request timestamp.
timestampRequest :: IO RequestTimestamp
timestampRequest = RequestTimestamp <$> getCurrentTime

requestExpired :: RequestTimestamp -> RequestExpiry -> UTCTime -> RequestExpired
requestExpired rt re now
  | (unRequestTimestamp rt) > now = NotYetValid
  | (expiresAt rt re) <= now      = TimeExpired
  | otherwise                     = TimeValid

expiresAt :: RequestTimestamp -> RequestExpiry -> UTCTime
expiresAt (RequestTimestamp rt) (RequestExpiry re) =
  addUTCTime (fromIntegral re) rt
