{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.IO.Zodiac.Raw.TSRP where

import           Disorder.Core.IO (testIO)
import           Disorder.Core.Property (failWith)
import           Disorder.Core.Run (ExpectedTestSpeed(..), disorderCheckEnvAll)

import           P

import           System.IO (IO)

import           Test.Zodiac.Core.Arbitrary ()
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()
import           Test.Zodiac.Core.Gen

import           Tinfoil.Data (Verified(..), SymmetricKey)

import           Zodiac.Core.Data
import           Zodiac.Raw.Error
import           Zodiac.Raw.Request
import           Zodiac.Raw.TSRP

prop_verifyRawRequest' :: KeyId
                       -> RequestTimestamp
                       -> RequestExpiry
                       -> CRequest
                       -> SymmetricKey
                       -> Property
prop_verifyRawRequest' kid rt re cr sk =
  forAll (genTimeWithin rt re) $ \now ->
    let req = fromCanonicalRequest cr in
    case authedRawRequest kid sk re req rt of
      Left e ->
        failWith $ "authentication unexpectedly failed: " <> renderRequestError e
      Right ar -> testIO $ do
        r <- verifyRawRequest' kid sk ar now
        pure $ r === Verified

return []
tests :: IO Bool
tests = $disorderCheckEnvAll TestRunMore
