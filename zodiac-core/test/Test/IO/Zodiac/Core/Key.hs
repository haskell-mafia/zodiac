{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.IO.Zodiac.Core.Key where

import           Disorder.Core.IO (testIO)
import           Disorder.Core.Property ((=/=))
import           Disorder.Core.Run (ExpectedTestSpeed(..), disorderCheckEnvAll)

import           P

import           System.IO (IO)

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           Zodiac.Core.Key

prop_genKeyId :: Property
prop_genKeyId = testIO $ do
  k1 <- genKeyId
  k2 <- genKeyId
  pure $ k1 =/= k2

return []
tests :: IO Bool
tests = $disorderCheckEnvAll TestRunMore
