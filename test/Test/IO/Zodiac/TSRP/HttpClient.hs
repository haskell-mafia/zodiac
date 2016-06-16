{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.IO.Zodiac.TSRP.HttpClient where

import           Disorder.Core.IO (testIO)
import           Disorder.Core.Property (failWith)
import           Disorder.Core.Run (ExpectedTestSpeed(..), disorderCheckEnvAll)

import           Network.HTTP.Client (requestHeaders)

import           P

import           System.IO (IO)

import           Test.Zodiac.Arbitrary ()
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           Tinfoil.Data (SymmetricKey)

import           Zodiac.Data
import           Zodiac.Request
import           Zodiac.Request.HttpClient
import           Zodiac.TSRP.HttpClient

-- FIXME: rewrite once https://github.com/ambiata/tinfoil/pull/47 is merged
prop_extractHttpClientAuthHeader :: CRequest
                                 -> KeyId
                                 -> SymmetricKey
                                 -> RequestExpiry
                                 -> RequestTimestamp
                                 -> Property
prop_extractHttpClientAuthHeader cr kid sk re rt =
  let req = fromCanonicalRequest cr
      mac = macHttpClientRequest kid sk re req rt in
  case mac of
    Left e -> failWith $ "authentication unexpectedly failed: " <> renderRequestError e
    Right mac' ->
      let sah = SymmetricAuthHeader TSRPv1 kid rt re (signedHeaders cr) mac'
          authH = httpAuthHeader TSRPv1 kid rt re cr mac'
          newHs = authH : (requestHeaders req)
          req' = req { requestHeaders = newHs } in
          case extractHttpClientAuthHeader req' of
            Left e ->
              failWith $ "auth header extraction unexpectedly failed: " <> renderProtocolError e
            Right sah' -> testIO $ do
              r <- sah' `symmetricAuthHeaderEq` sah
              pure $ r === True

return []
tests :: IO Bool
tests = $disorderCheckEnvAll TestRunMore
