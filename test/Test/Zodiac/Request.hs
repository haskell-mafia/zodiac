{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Zodiac.Request where

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

import           Zodiac.Data.Request
import           Zodiac.Request

prop_tripping_CRequest :: CRequest -> Property
prop_tripping_CRequest = tripping fromCanonicalRequest toCanonicalRequest

prop_trimSpaces :: ByteString -> Property
prop_trimSpaces bs =
  let bs' = BS.filter (/= 0x20) bs -- no spaces
      bs1 = trimSpaces $ " " <> bs' <> " "
      bs2 = trimSpaces $ bs' <> "   " <> bs' in
  case BS.null bs' of
    True -> bs1 === bs'
    False -> (bs1, bs2) === (bs', bs' <> " " <> bs')

prop_renderCRequest :: UniquePair CRequest -> Property
prop_renderCRequest (UniquePair cr1 cr2) =
  cr1 =/= cr2

return []
tests :: IO Bool
tests = $disorderCheckEnvAll TestRunMore
