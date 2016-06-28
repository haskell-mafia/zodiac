{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.IO.Zodiac.HttpClient.TSRP where

import           Disorder.Core.IO (testIO)
import           Disorder.Core.Property (failWith)
import           Disorder.Core.Run (ExpectedTestSpeed(..), disorderCheckEnvAll)

import           Network.HTTP.Client (requestHeaders)

import           P

import qualified Prelude

import           System.IO (IO)

import           Test.Zodiac.Core.Arbitrary ()
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()
import           Test.Zodiac.Core.Gen

import           Tinfoil.Data (Verified(..), SymmetricKey)

import           Zodiac.Core.Data
import           Zodiac.Core.Request
import           Zodiac.HttpClient.Error
import           Zodiac.HttpClient.Request
import           Zodiac.HttpClient.TSRP

-- FIXME: rewrite once https://github.com/ambiata/tinfoil/pull/47 is merged
prop_httpClientAuthHeader :: CRequest
                          -> KeyId
                          -> SymmetricKey
                          -> RequestExpiry
                          -> RequestTimestamp
                          -> Property
prop_httpClientAuthHeader cr kid sk re rt =
  let req = fromCanonicalRequest cr
      mac = macHttpClientRequest kid sk re req rt in
  case mac of
    Left e -> failWith $ "authentication unexpectedly failed: " <> renderRequestError e
    Right mac' ->
      let sah = SymmetricAuthHeader TSRPv1 kid rt re (signedHeaders cr) mac'
          authH = httpAuthHeader TSRPv1 kid rt re cr mac'
          newHs = authH : (requestHeaders req)
          req' = req { requestHeaders = newHs } in
          case httpClientAuthHeader req' of
            Left e ->
              failWith $ "auth header extraction unexpectedly failed: " <> renderProtocolError e
            Right sah' -> testIO $ do
              r <- sah' `symmetricAuthHeaderEq` sah
              pure $ r === True

prop_verifyHttpClientRequest' :: KeyId
                              -> RequestTimestamp
                              -> RequestExpiry
                              -> CRequest
                              -> SymmetricKey
                              -> Property
prop_verifyHttpClientRequest' kid rt re cr sk =
  forAll (genTimeWithin rt re) $ \now ->
    let req = fromCanonicalRequest cr in
    case authedHttpClientRequest kid sk re req rt of
      Left e ->
        failWith $ "authentication unexpectedly failed: " <> renderRequestError e
      Right ar -> testIO $ do
        r <- verifyHttpClientRequest' kid sk ar now
        pure $ r === Verified

-- FIXME: rewrite once https://github.com/ambiata/tinfoil/pull/47 is merged
prop_httpClientKeyId :: KeyId
                     -> RequestTimestamp
                     -> RequestExpiry
                     -> CRequest
                     -> SymmetricKey
                     -> Property
prop_httpClientKeyId kid rt re cr sk =
  let req = fromCanonicalRequest cr
      shs = signedHeaders cr
      mac = fromRight $ macHttpClientRequest kid sk re req rt
      sah = SymmetricAuthHeader TSRPv1 kid rt re shs mac
      ar = fromRight $ authedHttpClientRequest kid sk re req rt in
  case httpClientAuthHeader ar of
    Left e -> failWith $ "auth header lookup failed: " <> renderProtocolError e
    Right sah' -> testIO $ do
      r <- sah `symmetricAuthHeaderEq` sah'
      pure $ r === True
  where
    fromRight (Right x) = x
    fromRight (Left _) = Prelude.error "impossible: unexpected Left in prop_httpClientKeyId"

return []
tests :: IO Bool
tests = $disorderCheckEnvAll TestRunMore
