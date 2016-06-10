{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.IO.Zodiac.Symmetric where

import           Disorder.Core.IO (testIO)
import           Disorder.Core.Run (ExpectedTestSpeed(..), disorderCheckEnvAll)
import           Disorder.Core.UniquePair (UniquePair(..))

import           P

import           System.IO (IO)

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()
import           Test.Zodiac.Arbitrary ()
import           Test.Zodiac.Gen

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
  forAll (genTimeWithin rt re) $ \now ->
    let mac = macRequest sp hf kid rt re cr sk in testIO $ do
    r <- verifyRequest' sp hf kid rt re cr sk mac now
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
  forAll (genTimeWithin rt re) $ \now ->
    let mac = macRequest sp hf kid rt re cr sk1 in testIO $ do
    r <- verifyRequest' sp hf kid rt re cr sk2 mac now
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
  forAll (genTimeWithin rt re) $ \now ->
    let mac = macRequest sp hf kid rt re cr1 sk in testIO $ do
    r <- verifyRequest' sp hf kid rt re cr2 sk mac now
    pure $ r === NotVerified

prop_verifyRequest_before :: SymmetricProtocol
                          -> HashFunction
                          -> KeyId
                          -> RequestTimestamp
                          -> RequestExpiry
                          -> CRequest
                          -> SymmetricKey
                          -> Property
prop_verifyRequest_before sp hf kid rt re cr sk =
  forAll (genTimeBefore rt) $ \now ->
    let mac = macRequest sp hf kid rt re cr sk in testIO $ do
    r <- verifyRequest' sp hf kid rt re cr sk mac now
    pure $ r === NotVerified

prop_verifyRequest_after :: SymmetricProtocol
                          -> HashFunction
                          -> KeyId
                          -> RequestTimestamp
                          -> RequestExpiry
                          -> CRequest
                          -> SymmetricKey
                          -> Property
prop_verifyRequest_after sp hf kid rt re cr sk =
  forAll (genTimeExpired rt re) $ \now ->
    let mac = macRequest sp hf kid rt re cr sk in testIO $ do
    r <- verifyRequest' sp hf kid rt re cr sk mac now
    pure $ r === NotVerified

return []
tests :: IO Bool
tests = $disorderCheckEnvAll TestRunMore
