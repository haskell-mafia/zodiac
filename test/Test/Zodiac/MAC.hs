{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Zodiac.MAC where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import           Disorder.Core.Property ((=/=))
import           Disorder.Core.Run (ExpectedTestSpeed(..), disorderCheckEnvAll)
import           Disorder.Core.Tripping (tripping)
import           Disorder.Core.UniquePair (UniquePair(..))

import           P

import           System.IO (IO)

import           Test.Zodiac.Arbitrary ()
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           Zodiac.Data.MAC
import           Zodiac.MAC

prop_deriveRequestKey_timestamp :: UniquePair RequestTimestamp -> KeyId -> SymmetricKey -> Property
prop_deriveRequestKey_timestamp (UniquePair rt rt') kid sk =
  let k = deriveRequestKey rt kid sk
      k' = deriveRequestKey rt' kid sk in
  k =/= k'

prop_deriveRequestKey_key_id :: RequestTimestamp -> UniquePair KeyId -> SymmetricKey -> Property
prop_deriveRequestKey_key_id rt (UniquePair kid kid') sk =
  let k = deriveRequestKey rt kid sk
      k' = deriveRequestKey rt kid' sk in
  k =/= k'

prop_deriveRequestKey_key_id :: RequestTimestamp -> KeyId -> SymmetricKey -> Property
prop_deriveRequestKey_key_id rt kid (UniquePair sk sk') =
  let k = deriveRequestKey rt kid sk
      k' = deriveRequestKey rt kid' sk' in
  k =/= k'

return []
tests :: IO Bool
tests = $disorderCheckEnvAll TestRunMore
