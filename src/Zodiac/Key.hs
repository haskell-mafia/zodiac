{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Zodiac.Key(
    genKeyId
  , genSymmetricKey
  ) where

import           P

import           System.IO (IO)

import           Tinfoil.Data (Entropy(..), SymmetricKey)
import           Tinfoil.Key (genSymmetricKey256)
import           Tinfoil.Random (entropy)

import           Zodiac.Data.Key

-- | Generate a random key ID.
genKeyId :: IO KeyId
genKeyId = fmap (KeyId . unEntropy) $ entropy 16

-- | Generate a symmetric key for use in the TSRP protocol. This key
-- is shared between the server and client but kept secret from all
-- other parties.
genSymmetricKey :: IO SymmetricKey
genSymmetricKey = genSymmetricKey256
