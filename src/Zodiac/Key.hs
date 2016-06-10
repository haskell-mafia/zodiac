{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Zodiac.Key(
    genKeyId
  ) where

import           P

import           System.IO (IO)

import           Tinfoil.Data (Entropy(..))
import           Tinfoil.Random (entropy)

import           Zodiac.Data.Key

genKeyId :: IO KeyId
genKeyId = fmap (KeyId . unEntropy) $ entropy 16
