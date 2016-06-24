{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Zodiac.Core.Time where

import           Data.Time.Clock (addUTCTime)

import           Disorder.Core.Run (ExpectedTestSpeed(..), disorderCheckEnvAll)

import           P

import           Prelude (fromEnum)

import           System.IO (IO)

import           Test.Zodiac.Core.Arbitrary ()
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           Zodiac.Core.Data.Time
import           Zodiac.Core.Time

prop_requestExpired :: RequestTimestamp -> RequestExpiry -> Property
prop_requestExpired rt re =
  forAll (choose (1, unRequestExpiry re)) $ \secs ->
    let expiryTime = expiresAt rt re
        afterTime = addUTCTime (fromIntegral secs) expiryTime
        goodTime = addUTCTime (fromIntegral (- secs)) expiryTime
        rSame = requestExpired rt re expiryTime
        rAfter = requestExpired rt re afterTime
        rGood = requestExpired rt re goodTime in
  (rSame, rAfter, rGood) === (TimeExpired, TimeExpired, TimeValid)

prop_requestExpired_skew :: RequestTimestamp -> Property
prop_requestExpired_skew (RequestTimestamp now) =
  forAll (choose (fromEnum maxClockSkew, fromIntegral ((2 :: Int) ^ (32 :: Int)))) $ \skew ->
    forAll (choose (skew, maxBound :: Int)) $ \ex ->
      let re = RequestExpiry ex
          rt = RequestTimestamp . addUTCTime (fromIntegral skew) $ now
          r = requestExpired rt re now in
  r === NotYetValid

prop_expiresAt :: RequestTimestamp -> RequestExpiry -> Property
prop_expiresAt rt re =
  let et = expiresAt rt re in
  (et > (unRequestTimestamp rt)) === True

return []
tests :: IO Bool
tests = $disorderCheckEnvAll TestRunMore
