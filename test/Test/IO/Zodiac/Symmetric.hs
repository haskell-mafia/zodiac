{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.IO.Zodiac.Symmetric where

import           Disorder.Core.IO (testIO)
import           Disorder.Core.Run (ExpectedTestSpeed(..), disorderCheckEnvAll)
import           Disorder.Core.UniquePair (UniquePair(..))

import           P

import           System.IO (IO)

import           Test.Zodiac.Arbitrary ()
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           Tinfoil.Data

import           Zodiac.Data
import           Zodiac.Symmetric

prop_verifyRequest :: SymmetricProtocol
                   -> HashFunction
                   -> KeyId
                   -> RequestTimestamp
                   -> RequestExpiry
                   -> CRequest
                   -> SymmetricKey
                   -> Property
prop_verifyRequest sp hf kid rt re cr sk =
  let mac = macRequest sp hf kid rt re cr sk in testIO $ do
  r <- verifyRequest sp hf kid rt re cr sk mac
  pure $ r === Verified

prop_verifyRequest_key :: SymmetricProtocol
                       -> HashFunction
                       -> KeyId
                       -> RequestTimestamp
                       -> RequestExpiry
                       -> CRequest
                       -> UniquePair SymmetricKey
                       -> Property
prop_verifyRequest_key sp hf kid rt re cr (UniquePair sk1 sk2) =
  let mac = macRequest sp hf kid rt re cr sk1 in testIO $ do
  r <- verifyRequest sp hf kid rt re cr sk2 mac
  pure $ r === NotVerified

prop_verifyRequest_req :: SymmetricProtocol
                       -> HashFunction
                       -> KeyId
                       -> RequestTimestamp
                       -> RequestExpiry
                       -> UniquePair CRequest
                       -> SymmetricKey
                       -> Property
prop_verifyRequest_req sp hf kid rt re (UniquePair cr1 cr2) sk =
  let mac = macRequest sp hf kid rt re cr1 sk in testIO $ do
  r <- verifyRequest sp hf kid rt re cr2 sk mac
  pure $ r === NotVerified

return []
tests :: IO Bool
tests = $disorderCheckEnvAll TestRunMore
