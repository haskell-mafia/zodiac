{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Zodiac.Cli.TSRP.Error(
    TSRPError(..)
  , ParamError(..)

  , renderTSRPError
  ) where

import           P

data TSRPError =
    TSRPParamError !ParamError

renderTSRPError :: TSRPError -> Text
renderTSRPError (TSRPParamError e) = renderParamError e

data ParamError =
    MissingRequiredParam !Text
  | InvalidParam !Text

renderParamError :: ParamError -> Text
renderParamError (MissingRequiredParam p) =
  "missing required parameter: " <> p
renderParamError (InvalidParam p) =
  "invalid parameter: " <> p
