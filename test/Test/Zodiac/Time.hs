{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Zodiac.Time where

import           Data.Time.Clock (addUTCTime)

import           Disorder.Core.Run (ExpectedTestSpeed(..), disorderCheckEnvAll)

import           P

import           System.IO (IO)

import           Test.Zodiac.Arbitrary ()
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           Zodiac.Data.Time
import           Zodiac.Time

prop_requestExpired :: RequestTimestamp -> RequestExpiry -> Property
prop_requestExpired rt re =
  forAll (choose (1, unRequestExpiry re)) $ \secs ->
    let expiryTime = expiresAt rt re
        afterTime = addUTCTime (fromIntegral secs) expiryTime
        goodTime = addUTCTime (fromIntegral (- secs)) expiryTime
        beforeTime = addUTCTime (fromIntegral (- secs)) (unRequestTimestamp rt)
        rSame = requestExpired rt re expiryTime
        rAfter = requestExpired rt re afterTime
        rGood = requestExpired rt re goodTime
        rBefore = requestExpired rt re beforeTime in
  (rSame, rAfter, rGood, rBefore) === (TimeExpired, TimeExpired, TimeValid, NotYetValid)
      

prop_expiresAt :: RequestTimestamp -> RequestExpiry -> Property
prop_expiresAt rt re =
  let et = expiresAt rt re in
  (et > (unRequestTimestamp rt)) === True

return []
tests :: IO Bool
tests = $disorderCheckEnvAll TestRunMore
