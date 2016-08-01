{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Zodiac.TSRP.Key(
    genKeyId
  , genTSRPKey
  ) where

import           P

import           System.IO (IO)

import           Tinfoil.Data (Entropy(..))
import qualified Tinfoil.Key as Tinfoil
import           Tinfoil.Random (entropy)

import           Zodiac.TSRP.Data.Key

-- | Generate a random key ID.
genKeyId :: IO KeyId
genKeyId = fmap (KeyId . unEntropy) $ entropy 16

-- | Generate a symmetric key for use in the TSRP protocol. This key
-- is shared between the server and client but kept secret from all
-- other parties.
genTSRPKey :: IO TSRPKey
genTSRPKey = TSRPKey <$> Tinfoil.genSymmetricKey
