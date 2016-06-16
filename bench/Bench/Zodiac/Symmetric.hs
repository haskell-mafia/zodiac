{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Bench.Zodiac.Symmetric where

import           Bench.Zodiac

import           Criterion.Main

import           P

import           Test.QuickCheck (arbitrary)
import           Test.Zodiac.Arbitrary ()

import           Zodiac.Data
import           Zodiac.Symmetric

macRequestEnv = (,,,,) <$> arbitrary
                       <*> arbitrary
                       <*> arbitrary
                       <*> arbitrary
                       <*> arbitrary

benchmarks = [
    env (generate' macRequestEnv) $ \ ~(kid, rt, re, cr, sk) ->
      bgroup "Symmetric" $ [
          bench "macRequest" $ nf (macRequest TSRPv1 kid rt re cr) sk
        , bench "authenticationString" $ nf (authenticationString TSRPv1 kid rt re) cr
        ]
  ]
