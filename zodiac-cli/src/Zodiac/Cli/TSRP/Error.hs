{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Zodiac.Cli.TSRP.Error(
    TSRPError(..)
  , ParamError(..)

  , renderTSRPError
  ) where

import qualified Data.Text as T

import           P

import qualified Zodiac.Raw as Z

data TSRPError =
    TSRPNotImplementedError
  | TSRPParamError !ParamError
  | TSRPRequestError !Z.RequestError
  deriving (Eq, Show)

renderTSRPError :: TSRPError -> Text
renderTSRPError TSRPNotImplementedError = "Implement me!"
renderTSRPError (TSRPParamError e) = renderParamError e
renderTSRPError (TSRPRequestError e) = Z.renderRequestError e

data ParamError =
    MissingRequiredParam !Text
  | InvalidParam !Text !Text
  deriving (Eq, Show)

renderParamError :: ParamError -> Text
renderParamError (MissingRequiredParam p) =
  "missing required parameter: " <> p
renderParamError (InvalidParam var val) = T.unwords [
    "invalid parameter"
  , var
  , ": failed to parse"
  , val
  ]
