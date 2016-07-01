{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Zodiac.HttpClient.Request where

import           Disorder.Core.Run (ExpectedTestSpeed(..), disorderCheckEnvAll)
import           Disorder.Core.Tripping (tripping)

import           P

import           System.IO (IO)

import           Test.Zodiac.Core.Arbitrary ()
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           Zodiac.Core.Data.Request
import           Zodiac.HttpClient.Request

prop_tripping_CRequest :: CRequest -> Property
prop_tripping_CRequest = tripping fromCanonicalRequest toCanonicalRequest

return []
tests :: IO Bool
tests = $disorderCheckEnvAll TestRunMore
