{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Zodiac.Core.Header(
    trimSpaces
  ) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import           P

-- | Remove leading and trailing spaces, replace all internal strings
-- of spaces with a single space. This is a requirement of the header
-- value canonical form, so it is placed in 'Zodiac.Core' for
-- convenience of implementing conversion functions.
trimSpaces :: ByteString -> ByteString
trimSpaces x =
  let gps = foldSpace <$> BS.group x in
  trimTrailing . trimLeading $ BS.concat gps
  where
    trimTrailing bs = case BS.unsnoc bs of
      Nothing -> ""
      Just (xs, 0x20) -> xs
      Just (xs, y) -> BS.snoc xs y

    trimLeading bs = case BS.uncons bs of
      Nothing -> ""
      Just (0x20, xs) -> xs
      Just (y, xs) -> BS.cons y xs

    foldSpace bs = case BS.head bs of
      0x20 -> BS.singleton 0x20
      _ -> bs
