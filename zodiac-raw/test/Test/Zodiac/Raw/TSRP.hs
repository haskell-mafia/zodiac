{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Zodiac.Raw.TSRP where

import qualified Data.List.NonEmpty as NE
import           Disorder.Core.Property (failWith)
import           Disorder.Core.Run (ExpectedTestSpeed(..), disorderCheckEnvAll)

import qualified Hadron as H

import           P

import qualified Prelude

import           System.IO (IO)

import           Test.Zodiac.Core.Arbitrary ()
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           Tinfoil.Data (SymmetricKey)

import           Zodiac.Core.Data
import           Zodiac.Raw.Request
import           Zodiac.Raw.TSRP

prop_authedRawRequest :: CRequest
                      -> KeyId
                      -> SymmetricKey
                      -> RequestExpiry
                      -> RequestTimestamp
                      -> Property
prop_authedRawRequest cr kid sk re rt =
  let req = fromCanonicalRequest cr
      res = fromRight "authedRawRequest" $ authedRawRequest kid sk re req rt
      req' = fromRight "parseRawRequest" $ parseRawRequest res in
  case lookupHeader req' (H.HeaderName "authorization") of
    Nothing' -> failWith $ "no authentication header in authenticated request"
    Just' auths ->
      -- Tests can generate "authorization" as a header name, so we just
      -- want to make sure our one is in there.
      any validAuthHeader (NE.toList auths) === True
  where
    validAuthHeader (H.HeaderValue hv) = case parseSymmetricAuthHeader hv of
      Just' _ -> True
      Nothing' -> False

    fromRight msg (Left e) = Prelude.error $ msg <> ": " <> show e
    fromRight _ (Right x) = x

return []
tests :: IO Bool
tests = $disorderCheckEnvAll TestRunMore
