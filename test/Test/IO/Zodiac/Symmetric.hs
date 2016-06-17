{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.IO.Zodiac.Symmetric where

import           Data.Time.Clock (addUTCTime)

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
import           Zodiac.Request
import           Zodiac.Symmetric

prop_verifyRequest' :: SymmetricProtocol
                    -> KeyId
                    -> RequestTimestamp
                    -> RequestExpiry
                    -> CRequest
                    -> SymmetricKey
                    -> Property
prop_verifyRequest' sp kid rt re cr sk =
  forAll (genTimeWithin rt re) $ \now ->
    let mac = macRequest sp kid rt re cr sk in testIO $ do
    r <- verifyRequest' sp kid rt re (StrippedCRequest cr) sk mac now
    pure $ r === Verified

prop_verifyRequest_key' :: SymmetricProtocol
                        -> KeyId
                        -> RequestTimestamp
                        -> RequestExpiry
                        -> CRequest
                        -> UniquePair SymmetricKey
                        -> Property
prop_verifyRequest_key' sp kid rt re cr (UniquePair sk1 sk2) =
  forAll (genTimeWithin rt re) $ \now ->
    let mac = macRequest sp kid rt re cr sk1 in testIO $ do
    r <- verifyRequest' sp kid rt re (StrippedCRequest cr) sk2 mac now
    pure $ r === NotVerified

prop_verifyRequest_req' :: SymmetricProtocol
                        -> KeyId
                        -> RequestTimestamp
                        -> RequestExpiry
                        -> UniquePair CRequest
                        -> SymmetricKey
                        -> Property
prop_verifyRequest_req' sp kid rt re (UniquePair cr1 cr2) sk =
  forAll (genTimeWithin rt re) $ \now ->
    let mac = macRequest sp kid rt re cr1 sk in testIO $ do
    r <- verifyRequest' sp kid rt re (StrippedCRequest cr2) sk mac now
    pure $ r === NotVerified

prop_verifyRequest_before' :: SymmetricProtocol
                           -> KeyId
                           -> RequestTimestamp
                           -> RequestExpiry
                           -> CRequest
                           -> SymmetricKey
                           -> Property
prop_verifyRequest_before' sp kid rt re cr sk =
  forAll (genTimeBefore (RequestTimestamp $ addUTCTime (- maxClockSkew) (unRequestTimestamp rt))) $ \now ->
    let mac = macRequest sp kid rt re cr sk in testIO $ do
    r <- verifyRequest' sp kid rt re (StrippedCRequest cr) sk mac now
    pure $ r === NotVerified

prop_verifyRequest_after' :: SymmetricProtocol
                          -> KeyId
                          -> RequestTimestamp
                          -> RequestExpiry
                          -> CRequest
                          -> SymmetricKey
                          -> Property
prop_verifyRequest_after' sp kid rt re cr sk =
  forAll (genTimeExpired rt re) $ \now ->
    let mac = macRequest sp kid rt re cr sk in testIO $ do
    r <- verifyRequest' sp kid rt re (StrippedCRequest cr) sk mac now
    pure $ r === NotVerified

prop_verifyRequest :: SymmetricProtocol
                   -> KeyId
                   -> RequestTimestamp
                   -> RequestExpiry
                   -> CRequest
                   -> SymmetricKey
                   -> Property
prop_verifyRequest sp kid rt re cr sk =
  forAll (genTimeWithin rt re) $ \now ->
    let mac = macRequest sp kid rt re cr sk
        shs = signedHeaders cr
        sah = SymmetricAuthHeader sp kid rt re shs mac in testIO $ do
    r <- verifyRequest kid sk cr sah now
    pure $ r === Verified

prop_verifyRequest_kid :: SymmetricProtocol
                       -> UniquePair KeyId
                       -> RequestTimestamp
                       -> RequestExpiry
                       -> CRequest
                       -> SymmetricKey
                       -> Property
prop_verifyRequest_kid sp (UniquePair kid kid') rt re cr sk =
  forAll (genTimeWithin rt re) $ \now ->
    let mac = macRequest sp kid rt re cr sk
        shs = signedHeaders cr
        sah = SymmetricAuthHeader sp kid' rt re shs mac in testIO $ do
    r <- verifyRequest kid sk cr sah now
    pure $ r === NotVerified

return []
tests :: IO Bool
tests = $disorderCheckEnvAll TestRunMore
