{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Zodiac.Data.Time where

import           Disorder.Core.Run (ExpectedTestSpeed(..), disorderCheckEnvAll)
import           Disorder.Core.Tripping (tripping)

import           P

import           System.IO (IO)

import           Zodiac.Data.Time

import           Test.Zodiac.Arbitrary ()
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

prop_tripping_RequestTimestamp :: RequestTimestamp -> Property
prop_tripping_RequestTimestamp = tripping renderRequestTimestamp parseRequestTimestamp

prop_tripping_RequestExpiry :: RequestExpiry -> Property
prop_tripping_RequestExpiry = tripping renderRequestExpiry parseRequestExpiry

return []
tests :: IO Bool
tests = $disorderCheckEnvAll TestRunMore
