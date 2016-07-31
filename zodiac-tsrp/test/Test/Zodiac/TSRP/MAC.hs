{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Zodiac.TSRP.MAC where

import           Disorder.Core.Property ((=/=))
import           Disorder.Core.Run (ExpectedTestSpeed(..), disorderCheckEnvAll)
import           Disorder.Core.UniquePair (UniquePair(..))

import           P

import           System.IO (IO)

import           Test.Zodiac.TSRP.Arbitrary ()
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           Tinfoil.Data.Key (SymmetricKey(..))

import           Zodiac.TSRP.Data
import           Zodiac.TSRP.MAC

prop_deriveRequestKey_timestamp :: SymmetricProtocol -> UniquePair RequestDate -> KeyId -> SymmetricKey -> Property
prop_deriveRequestKey_timestamp p (UniquePair rt rt') kid sk =
  let k = deriveRequestKey p rt kid sk
      k' = deriveRequestKey p rt' kid sk in
  k =/= k'

prop_deriveRequestKey_key_id :: SymmetricProtocol -> RequestDate -> UniquePair KeyId -> SymmetricKey -> Property
prop_deriveRequestKey_key_id p rt (UniquePair kid kid') sk =
  let k = deriveRequestKey p rt kid sk
      k' = deriveRequestKey p rt kid' sk in
  k =/= k'

prop_deriveRequestKey_key :: SymmetricProtocol -> RequestDate -> KeyId -> UniquePair (Blind SymmetricKey) -> Property
prop_deriveRequestKey_key p rt kid (UniquePair (Blind sk) (Blind sk')) =
  let k = deriveRequestKey p rt kid sk
      k' = deriveRequestKey p rt kid sk' in
  k =/= k'

return []
tests :: IO Bool
tests = $disorderCheckEnvAll TestRunMore
