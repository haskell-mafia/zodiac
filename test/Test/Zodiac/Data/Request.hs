{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Zodiac.Data.Request where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

import           Disorder.Core.Tripping (tripping)

import           P

import           System.IO (IO)

import           Zodiac.Data.Request

import           Test.Zodiac.Arbitrary ()
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

prop_renderCURI :: ByteString -> Property
prop_renderCURI bs =
  let bs' = renderCURI $ CURI bs in
  urlEncoded bs'

prop_renderCQueryString :: ByteString -> Property
prop_renderCQueryString bs =
  let bs' = renderCQueryString $ CQueryString bs in
  urlEncoded bs'

prop_tripping_CMethod :: CMethod -> Property
prop_tripping_CMethod = tripping renderCMethod parseCMethod

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 1000 } )
