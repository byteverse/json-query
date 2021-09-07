{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Json.Error
  ( -- * Types
    Error(..)
    -- * Encoding
  , encode
  , builderUtf8
  ) where

import Data.Bytes.Builder (Builder)
import Data.Primitive (SmallArray)
import Data.Text.Short (ShortText)
import Json.Context (Context(..))
import Data.ByteString.Short.Internal (ShortByteString(SBS))
import Control.Monad.ST (runST)

import qualified Data.Bytes.Builder as Builder
import qualified Data.Bytes.Chunks as ByteChunks
import qualified Data.Primitive as PM
import qualified Data.Primitive.Contiguous as Arr
import qualified Data.Text.Short.Unsafe as TS
import qualified GHC.Exts as Exts
import qualified Json.Context as Context
import qualified Json.Path as Path

-- | A single error message.
data Error = Error
  { message :: !ShortText
  , context :: !Context
  } deriving (Eq,Show)

ba2st :: PM.ByteArray -> ShortText
{-# inline ba2st #-}
ba2st (PM.ByteArray x) = TS.fromShortByteStringUnsafe (SBS x)

encode :: Error -> ShortText
encode p = ba2st (ByteChunks.concatU (Builder.run 128 (builderUtf8 p)))

builderUtf8 :: Error -> Builder
builderUtf8 Error{message,context}
  =  Context.builderUtf8 context
  <> Builder.ascii2 ':' ' '
  <> Builder.shortTextUtf8 message


