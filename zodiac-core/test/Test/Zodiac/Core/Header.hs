{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Zodiac.Core.Header where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import           Disorder.Core.Run (ExpectedTestSpeed(..), disorderCheckEnvAll)

import           P

import           System.IO (IO)

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           Zodiac.Core.Header

prop_trimSpaces :: ByteString -> Property
prop_trimSpaces bs =
  let bs' = BS.filter (/= 0x20) bs -- no spaces
      bs1 = trimSpaces $ " " <> bs' <> " "
      bs2 = trimSpaces $ bs' <> "   " <> bs' in
  case BS.null bs' of
    True -> bs1 === bs'
    False -> (bs1, bs2) === (bs', bs' <> " " <> bs')

return []
tests :: IO Bool
tests = $disorderCheckEnvAll TestRunMore
