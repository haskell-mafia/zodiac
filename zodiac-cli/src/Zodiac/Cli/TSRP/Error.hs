{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Zodiac.Cli.TSRP.Error(
    TSRPError(..)
  , ParamError(..)

  , renderTSRPError
  ) where

import qualified Data.Text as T

import           P

data TSRPError =
    TSRPParamError !ParamError

renderTSRPError :: TSRPError -> Text
renderTSRPError (TSRPParamError e) = renderParamError e

data ParamError =
    MissingRequiredParam !Text
  | InvalidParam !Text !Text

renderParamError :: ParamError -> Text
renderParamError (MissingRequiredParam p) =
  "missing required parameter: " <> p
renderParamError (InvalidParam var val) = T.unwords [
    "invalid parameter"
  , var
  , ": failed to parse"
  , val
  ]
