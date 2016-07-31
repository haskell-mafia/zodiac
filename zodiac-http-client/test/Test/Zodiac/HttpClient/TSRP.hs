{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Zodiac.HttpClient.TSRP where

import           Disorder.Core.Property (failWith)
import           Disorder.Core.Run (ExpectedTestSpeed(..), disorderCheckEnvAll)

import           Network.HTTP.Client (requestHeaders)

import           P

import           System.IO (IO)

import           Test.Zodiac.TSRP.Arbitrary ()
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           Tinfoil.Data (SymmetricKey)

import           Zodiac.TSRP.Data
import           Zodiac.HttpClient.Error
import           Zodiac.HttpClient.Request
import           Zodiac.HttpClient.TSRP

prop_authedHttpClientRequest :: CRequest
                             -> KeyId
                             -> SymmetricKey
                             -> RequestExpiry
                             -> RequestTimestamp
                             -> Property
prop_authedHttpClientRequest cr kid sk re rt =
  let req = fromCanonicalRequest cr
      res = authedHttpClientRequest kid sk re req rt in
  case res of
    Left e -> failWith $ "authentication unexpectedly failed: " <> renderRequestError e
    Right req' ->
      let hs = requestHeaders req'
          ahs = filter ((== "authorization") . fst) hs in
      case ahs of
        [] -> failWith $ "no authentication header in authenticated request"
        auths ->
          -- Tests can generate "authorization" as a header name, so we just
          -- want to make sure our one is in there.
          any validAuthHeader auths === True
  where
    validAuthHeader (_hn, hv) = case parseSymmetricAuthHeader hv of
      Just' _ -> True
      Nothing' -> False

return []
tests :: IO Bool
tests = $disorderCheckEnvAll TestRunMore
