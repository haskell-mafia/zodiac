{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Zodiac.Core.Data.Time where

import           Disorder.Core.Run (ExpectedTestSpeed(..), disorderCheckEnvAll)
import           Disorder.Core.Tripping (tripping)

import           P

import           System.IO (IO)

import           Zodiac.Core.Data.Time

import           Test.Zodiac.Core.Arbitrary ()
import           Test.Zodiac.Core.Gen
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

prop_tripping_RequestTimestamp :: RequestTimestamp -> Property
prop_tripping_RequestTimestamp = tripping renderRequestTimestamp parseRequestTimestamp

prop_parseRequestTimestamp_neg :: Property
prop_parseRequestTimestamp_neg = forAll genNegativeTimestamp $ \bs ->
  (parseRequestTimestamp bs) === Nothing'

prop_tripping_RequestExpiry :: RequestExpiry -> Property
prop_tripping_RequestExpiry = tripping renderRequestExpiry parseRequestExpiry

prop_parseRequestExpiry_neg :: Property
prop_parseRequestExpiry_neg = forAll genInvalidExpiry $ \e ->
  (parseRequestExpiry e) === Nothing'

return []
tests :: IO Bool
tests = $disorderCheckEnvAll TestRunMore
