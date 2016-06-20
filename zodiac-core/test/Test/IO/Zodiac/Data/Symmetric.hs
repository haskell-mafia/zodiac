{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.IO.Zodiac.Data.Symmetric where

import           Disorder.Core.IO (testIO)
import           Disorder.Core.Property (failWith)
import           Disorder.Core.Run (ExpectedTestSpeed(..), disorderCheckEnvAll)
import           P

import           System.IO (IO)

import           Zodiac.Core.Data.Symmetric

import           Test.Zodiac.Arbitrary ()
import           Test.QuickCheck

-- really need a SafeEq typeclass for this kind of thing
-- FIXME: rewrite once https://github.com/ambiata/tinfoil/pull/47 is merged
prop_tripping_SymmetricAuthHeader :: SymmetricAuthHeader -> Property
prop_tripping_SymmetricAuthHeader sah =
  let sah' = parseSymmetricAuthHeader $ renderSymmetricAuthHeader sah in
  case sah' of
    Nothing' -> failWith "parsing SymmetricAuthHeader unexpectedly failed"
    Just' sah'' -> testIO $ do
      r <- sah'' `symmetricAuthHeaderEq` sah
      pure $ r === True

return []
tests :: IO Bool
tests = $disorderCheckEnvAll TestRunMore
