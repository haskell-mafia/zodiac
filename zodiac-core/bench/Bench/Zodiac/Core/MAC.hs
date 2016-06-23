{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Bench.Zodiac.Core.MAC where

import           Bench.Zodiac

import           Criterion.Main

import           P

import           Test.QuickCheck (arbitrary)
import           Test.Zodiac.Core.Arbitrary ()

import           Zodiac.Core.Data
import           Zodiac.Core.MAC

macEnv = (,,) <$> arbitrary
                <*> arbitrary
                <*> arbitrary

benchmarks = [
    env (generate' macEnv) $ \ ~(rd, kid, sk) ->
      bgroup "MAC" $ [
          bench "deriveRequestKey" $ nf (deriveRequestKey TSRPv1 rd kid) sk
        ]
  ]
