{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Zodiac.Request where

import           Disorder.Core.Property ((=/=))
import           Disorder.Core.Run (ExpectedTestSpeed(..), disorderCheckEnvAll)
import           Disorder.Core.UniquePair (UniquePair(..))

import           P

import           System.IO (IO)

import           Test.Zodiac.Arbitrary ()
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           Zodiac.Data.Request
import           Zodiac.Request

prop_renderCRequest :: UniquePair CRequest -> Property
prop_renderCRequest (UniquePair cr1 cr2) =
  renderCRequest cr1 =/= renderCRequest cr2

return []
tests :: IO Bool
tests = $disorderCheckEnvAll TestRunMore
