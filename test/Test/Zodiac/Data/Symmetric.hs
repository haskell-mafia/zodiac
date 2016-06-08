{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Zodiac.Data.Symmetric where

import           Disorder.Core.Run (ExpectedTestSpeed(..), disorderCheckEnvAll)
import           Disorder.Core.Tripping (tripping)

import           P

import           System.IO (IO)

import           Zodiac.Data.Symmetric

import           Test.Zodiac.Arbitrary ()
import           Test.QuickCheck

prop_tripping_SymmetricAuthHeader :: SymmetricAuthHeader -> Property
prop_tripping_SymmetricAuthHeader = tripping renderSymmetricAuthHeader parseSymmetricAuthHeader

return []
tests :: IO Bool
tests = $disorderCheckEnvAll TestRunMore
