{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Bench.Zodiac.TSRP.MAC where

import           Bench.Zodiac

import           Criterion.Main

import           P

import           Test.QuickCheck (arbitrary)
import           Test.Zodiac.TSRP.Arbitrary ()

import           Zodiac.TSRP.Data
import           Zodiac.TSRP.MAC

macEnv = (,,) <$> arbitrary
                <*> arbitrary
                <*> arbitrary

benchmarks = [
    env (generate' macEnv) $ \ ~(rd, kid, sk) ->
      bgroup "MAC" $ [
          bench "deriveRequestKey" $ nf (deriveRequestKey TSRPv1 rd kid) sk
        ]
  ]
