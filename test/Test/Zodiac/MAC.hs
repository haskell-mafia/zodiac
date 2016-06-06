{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Zodiac.MAC where

import           Disorder.Core.Property ((=/=))
import           Disorder.Core.Run (ExpectedTestSpeed(..), disorderCheckEnvAll)
import           Disorder.Core.UniquePair (UniquePair(..))

import           P

import           System.IO (IO)

import           Test.Zodiac.Arbitrary ()
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           Tinfoil.Data.Key (SymmetricKey(..))

import           Zodiac.Data.Key
import           Zodiac.Data.Request
import           Zodiac.MAC

prop_deriveRequestKey_timestamp :: UniquePair RequestDate -> KeyId -> SymmetricKey -> Property
prop_deriveRequestKey_timestamp (UniquePair rt rt') kid sk =
  let k = deriveRequestKey rt kid sk
      k' = deriveRequestKey rt' kid sk in
  k =/= k'

prop_deriveRequestKey_key_id :: RequestDate -> UniquePair KeyId -> SymmetricKey -> Property
prop_deriveRequestKey_key_id rt (UniquePair kid kid') sk =
  let k = deriveRequestKey rt kid sk
      k' = deriveRequestKey rt kid' sk in
  k =/= k'

prop_deriveRequestKey_key :: RequestDate -> KeyId -> UniquePair (Blind SymmetricKey) -> Property
prop_deriveRequestKey_key rt kid (UniquePair (Blind sk) (Blind sk')) =
  let k = deriveRequestKey rt kid sk
      k' = deriveRequestKey rt kid sk' in
  k =/= k'

return []
tests :: IO Bool
tests = $disorderCheckEnvAll TestRunMore
