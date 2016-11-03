{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Zodiac.Cli.ReqTool.Error(
    ReqToolError(..)

  , renderReqToolError
  ) where

import           P

import qualified Zodiac.Raw as Z

data ReqToolError =
    ReqToolRequestError !Z.RequestError
  deriving (Eq, Show)

renderReqToolError :: ReqToolError -> Text
renderReqToolError (ReqToolRequestError e) = Z.renderRequestError e
