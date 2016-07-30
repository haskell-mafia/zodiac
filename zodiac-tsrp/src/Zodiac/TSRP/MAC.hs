{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-
HMAC functionality for symmetric request-signing.

Don't use this module directly unless you know what you're doing.
-}
module Zodiac.TSRP.MAC(
    deriveRequestKey
  ) where

import           P

import           Tinfoil.Data (SymmetricKey(..), MAC(..))
import           Tinfoil.MAC (hmacSHA256)

import           Zodiac.TSRP.Data

-- | Derive the key we actually use to authenticate the request from the
-- secret key, the key ID and the date part of the request timestamp
-- via an iterated chain of keyed hashes.
deriveRequestKey :: SymmetricProtocol -> RequestDate -> KeyId -> SymmetricKey -> SymmetricKey
deriveRequestKey TSRPv1 rd (KeyId kid) (SymmetricKey sk) =
  let k0 = SymmetricKey $ sk <> renderRequestDate rd
      k1 = chainNext $ hmacSHA256 k0 kid
      k2 = chainNext $ hmacSHA256 k1 proto in
  k2
  where
    proto = renderSymmetricProtocol TSRPv1

    chainNext = SymmetricKey . unMAC
