{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Zodiac.Data.Protocol where

import           Disorder.Core.Run (ExpectedTestSpeed(..), disorderCheckEnvAll)
import           Disorder.Core.Tripping (tripping)

import           P

import           System.IO (IO)

import           Zodiac.Data.Protocol

import           Test.Zodiac.Arbitrary ()
import           Test.QuickCheck

prop_tripping_Protocol :: Protocol -> Property
prop_tripping_Protocol = tripping renderProtocol parseProtocol

return []
tests :: IO Bool
tests = $disorderCheckEnvAll TestRunMore
