{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Zodiac.Request where

import qualified Data.Map.Strict as M

import           Disorder.Core.Property ((=/=))
import           Disorder.Core.Run (ExpectedTestSpeed(..), disorderCheckEnvAll)
import           Disorder.Core.UniquePair (UniquePair(..))

import           P

import           System.IO (IO)

import           Test.Zodiac.Arbitrary ()
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           Zodiac.Core.Data.Request
import           Zodiac.Core.Request

prop_renderCRequest :: UniquePair CRequest -> Property
prop_renderCRequest (UniquePair cr1 cr2) =
  renderCRequest cr1 =/= renderCRequest cr2

prop_stripCRequest :: CRequest -> CHeaderName -> CHeaderValue -> Property
prop_stripCRequest cr hn hv =
  let shs = signedHeaders cr
      oldHeaders = unCHeaders $ crHeaders cr
      newHeaders = CHeaders $ M.insert hn (pure hv) oldHeaders
      cr' = cr { crHeaders = newHeaders }
      cr'' = unStrippedCRequest $ stripCRequest cr' shs in
  not (M.member hn oldHeaders) ==> cr === cr''

return []
tests :: IO Bool
tests = $disorderCheckEnvAll TestRunMore
