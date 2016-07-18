{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Zodiac.Export.Time where

import           Disorder.Core.Run (ExpectedTestSpeed(..), disorderCheckEnvAll)
import           Disorder.Core.Tripping (tripping)

import           P

import           System.IO (IO)

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()
import           Test.Zodiac.Core.Arbitrary ()
import           Test.Zodiac.Export.Gen

import           Zodiac.Core.Data.Time
import           Zodiac.Export.Time

prop_tripping_RequestTimestamp :: RequestTimestamp -> Property
prop_tripping_RequestTimestamp =
  tripping timestampToCTime (Just' . cTimeToTimestamp)

prop_tripping_CTime :: Property
prop_tripping_CTime = forAll genCTime $
  tripping cTimeToTimestamp (Just' . timestampToCTime)

return []
tests :: IO Bool
tests = $disorderCheckEnvAll TestRunMore
