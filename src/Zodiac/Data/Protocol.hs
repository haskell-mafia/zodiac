{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Zodiac.Data.Protocol(
    Protocol(..)
  , parseProtocol
  , renderProtocol
  ) where

import           Control.DeepSeq.Generics (genericRnf)

import           Data.ByteString (ByteString)

import           GHC.Generics (Generic)

import           P

-- | Identifier for a protocol for authenticating requests.
data Protocol =
    -- | Trivial Symmetric Signing Protocol version 1.
    TSRPv1
  deriving (Eq, Show, Generic, Enum, Bounded)

instance NFData Protocol where rnf = genericRnf

renderProtocol :: Protocol -> ByteString
renderProtocol TSRPv1 = "TSRPv1"

parseProtocol :: ByteString -> Maybe' Protocol
parseProtocol "TSRPv1" = Just' TSRPv1
parseProtocol _ = Nothing'
