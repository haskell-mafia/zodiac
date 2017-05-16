{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Zodiac.HttpClient.Request where

import           Disorder.Core.Run (ExpectedTestSpeed(..), disorderCheckEnvAll)
import           Disorder.Core.Tripping (tripping)

import qualified Network.HTTP.Client as HC

import           P

import           System.IO (IO)

import           Test.Zodiac.Core.Arbitrary ()
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           Zodiac.Core.Data.Request
import           Zodiac.HttpClient.Request

prop_tripping_CRequest :: CRequest -> Property
prop_tripping_CRequest = tripping fromCanonicalRequest toCanonicalRequest

prop_toCanonicalRequest_empty_path:: CRequest -> Property
prop_toCanonicalRequest_empty_path r =
  let
    r0 = fromCanonicalRequest r
    r1 = r0 { HC.path = "" }
    r2 = r0 { HC.path = "/" }
    r' = r { crURI = CURI "/" }
  in
  conjoin [
      toCanonicalRequest r1 === toCanonicalRequest r2
    , toCanonicalRequest r1 === Right r'
    ]

return []
tests :: IO Bool
tests = $disorderCheckEnvAll TestRunMore
