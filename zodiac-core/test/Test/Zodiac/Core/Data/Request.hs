{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Zodiac.Core.Data.Request where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

import           Disorder.Core.Run (ExpectedTestSpeed(..), disorderCheckEnvAll)
import           Disorder.Core.Tripping (tripping)

import           Network.HTTP.Types.URI (urlDecode)

import           P

import           System.IO (IO)

import           Zodiac.Core.Data.Request

import           Test.Zodiac.Core.Arbitrary ()
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

-- Ensure a URI component (path or query string) doesn't contain bad bytes.
urlEncoded :: ByteString -> Property
urlEncoded = (=== True) . BS.all (not . flip elem urlBad)
  where
    urlBad = join [
        control
      , unwise
      , pure 0x20 -- space
      ]

    -- ASCII control characters.
    control = 0x7f : [0x00..0x1f]

    -- "unwise" per RFC 2396
    unwise = BS.unpack . BSC.pack $ "{}|\\^[]`"

prop_encodeCURI :: ByteString -> Property
prop_encodeCURI bs =
  let bs' = unCURI $ encodeCURI bs in
  urlEncoded bs'

prop_tripping_encodeCURI :: ByteString -> Property
prop_tripping_encodeCURI =
  tripping encodeCURI (Just . urlDecode False . renderCURI)

prop_encodeCQueryString :: ByteString -> Property
prop_encodeCQueryString bs =
  let bs' = unCQueryString $ encodeCQueryString bs in
  urlEncoded bs'

prop_tripping_encodeCQueryString :: ByteString -> Property
prop_tripping_encodeCQueryString =
  tripping encodeCQueryString (Just . urlDecode True . renderCQueryString)

prop_tripping_CMethod :: CMethod -> Property
prop_tripping_CMethod = tripping renderCMethod parseCMethod

prop_tripping_CSignedHeaders :: CSignedHeaders -> Property
prop_tripping_CSignedHeaders = tripping renderCSignedHeaders parseCSignedHeaders

return []
tests :: IO Bool
tests = $disorderCheckEnvAll TestRunMore
