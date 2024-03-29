{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Json.Error
  ( -- * Types
    Error (..)

    -- * Encoding
  , encode
  , builderUtf8
  ) where

import Data.ByteString.Short.Internal (ShortByteString (SBS))
import Data.Bytes.Builder (Builder)
import Data.Text.Short (ShortText)
import Json.Context (Context (..))

import qualified Data.Bytes.Builder as Builder
import qualified Data.Bytes.Chunks as ByteChunks
import qualified Data.Primitive as PM
import qualified Data.Text.Short.Unsafe as TS
import qualified Json.Context as Context

-- | A single error message.
data Error = Error
  { message :: !ShortText
  , context :: !Context
  }
  deriving (Eq, Show)

ba2st :: PM.ByteArray -> ShortText
{-# INLINE ba2st #-}
ba2st (PM.ByteArray x) = TS.fromShortByteStringUnsafe (SBS x)

encode :: Error -> ShortText
encode p = ba2st (ByteChunks.concatU (Builder.run 128 (builderUtf8 p)))

builderUtf8 :: Error -> Builder
builderUtf8 Error {message, context} =
  Context.builderUtf8 context
    <> Builder.ascii2 ':' ' '
    <> Builder.shortTextUtf8 message
