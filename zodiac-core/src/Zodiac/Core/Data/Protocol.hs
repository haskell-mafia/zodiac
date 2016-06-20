{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Zodiac.Core.Data.Protocol(
    AsymmetricProtocol(..)
  , Protocol(..)
  , SymmetricProtocol(..)
  , authHeaderName
  , parseAsymmetricProtocol
  , parseProtocol
  , parseSymmetricProtocol
  , renderAsymmetricProtocol
  , renderProtocol
  , renderSymmetricProtocol
  ) where

import           Control.DeepSeq.Generics (genericRnf)

import           Data.ByteString (ByteString)

import           GHC.Generics (Generic)

import           P

data Protocol =
    Symmetric !SymmetricProtocol
  | Asymmetric !AsymmetricProtocol
  deriving (Eq, Show, Generic)

instance NFData Protocol where rnf = genericRnf

renderProtocol :: Protocol -> ByteString
renderProtocol (Symmetric p) = renderSymmetricProtocol p
renderProtocol (Asymmetric p) = renderAsymmetricProtocol p

parseProtocol :: ByteString -> Maybe' Protocol
parseProtocol t =
  maybe' (Asymmetric <$> parseAsymmetricProtocol t) (Just' . Symmetric) $ parseSymmetricProtocol t

-- | Identifier for a protocol for authenticating requests via message
-- authentication code (MAC) using a shared secret symmetric key.
data SymmetricProtocol =
    -- | Trivial Symmetric Signing Protocol version 1.
    TSRPv1
  deriving (Eq, Show, Generic, Enum, Bounded)

instance NFData SymmetricProtocol where rnf = genericRnf

renderSymmetricProtocol :: SymmetricProtocol -> ByteString
renderSymmetricProtocol TSRPv1 = "TSRPv1"

parseSymmetricProtocol :: ByteString -> Maybe' SymmetricProtocol
parseSymmetricProtocol "TSRPv1" = Just' TSRPv1
parseSymmetricProtocol _ = Nothing'

-- | Identifier for a protocol for authenticating requests via
-- cryptographic signature using a public/private keypair.
data AsymmetricProtocol =
    -- | Trivial Asymmetric Signing Protocol version 1.
    TARPv1
  deriving (Eq, Show, Generic, Enum, Bounded)

instance NFData AsymmetricProtocol where rnf = genericRnf

renderAsymmetricProtocol :: AsymmetricProtocol -> ByteString
renderAsymmetricProtocol TARPv1 = "TARPv1"

parseAsymmetricProtocol :: ByteString -> Maybe' AsymmetricProtocol
parseAsymmetricProtocol "TARPv1" = Just' TARPv1
parseAsymmetricProtocol _ = Nothing'

-- | The header we use for authentication details.
--
-- Yes, I know.
authHeaderName :: Text
authHeaderName = "authorization"
