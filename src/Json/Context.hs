{-# language BangPatterns #-}

module Json.Context
  ( Context(..)
    -- * Encoding
  , builderUtf8
    -- * Conversion
  , toPath
  ) where

import Json.Path (Path)
import Data.Text.Short (ShortText)
import Data.Bytes.Builder (Builder)

import qualified Json.Path as Path

-- | A finger into a json value indicating where a parser is
-- currently operating. When a parser focuses on a key-value
-- pair in a map, it adds 'Key' constructor to the context, and
-- when it focuses on an element of an array, it adds an 'Index'
-- constructor. Like all zipper-like data structures, it is, in
-- a sense, reversed, which makes it cheap to construct while
-- parsing.
data Context
  = Top
  | Key !ShortText !Context
  | Index !Int !Context
  deriving (Eq,Show)

-- | Reverse the context, converting it to a 'Path'.
-- For example, toPath performs this conversion:
--
-- > 12.bar.foo.Top ==> foo.bar.12.Nil
toPath :: Context -> Path
toPath = go Path.Nil where
  go !acc Top = acc
  go !acc (Key k xs) = go (Path.Key k acc) xs
  go !acc (Index i xs) = go (Path.Index i acc) xs

-- | Convert 'Context' to textual representation using UTF-8 as the encoding
-- scheme. This reverses the context to present it in the expected order.
builderUtf8 :: Context -> Builder
builderUtf8 ctx0 = Path.builderUtf8 (toPath ctx0)
