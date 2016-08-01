{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Zodiac.TSRP.Data.Key where

import           Disorder.Core (tripping)
import           Disorder.Core.Run (ExpectedTestSpeed(..), disorderCheckEnvAll)

import           P

import           System.IO (IO)

import           Test.Zodiac.TSRP.Arbitrary ()
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           Zodiac.TSRP.Data.Key

prop_tripping_KeyId :: KeyId -> Property
prop_tripping_KeyId = tripping renderKeyId parseKeyId

prop_tripping_TSRPKey :: TSRPKey -> Property
prop_tripping_TSRPKey = tripping renderTSRPKey parseTSRPKey

return []
tests :: IO Bool
tests = $disorderCheckEnvAll TestRunMore
