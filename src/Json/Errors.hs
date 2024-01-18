{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Json.Errors
  ( -- * Types
    Errors

    -- * Encoding
  , encode
  , builderUtf8
  , hPut

    -- * Create
  , singleton

    -- * Conversion
  , toSmallArray
  ) where

import Control.Monad.ST (runST)
import Data.ByteString.Short.Internal (ShortByteString (SBS))
import Data.Bytes.Builder (Builder)
import Data.Primitive (SmallArray)
import Data.Text.Short (ShortText)
import Json.Error (Error)
import System.IO (Handle)

import qualified Data.Bytes.Builder as Builder
import qualified Data.Bytes.Chunks as ByteChunks
import qualified Data.Primitive as PM
import qualified Data.Primitive.Contiguous as Arr
import qualified Data.Text.Short.Unsafe as TS
import qualified GHC.Exts as Exts
import qualified Json.Error as Error

-- | A builder for errors that support efficient concatenation.
data Errors
  = ErrorsOne !Error
  | ErrorsPlus !Errors !Errors

instance Semigroup Errors where
  (<>) = ErrorsPlus

instance Show Errors where
  showsPrec d x = showsPrec d (Exts.toList (toSmallArray x))

instance Eq Errors where
  x == y = toSmallArray x == toSmallArray y

singleton :: Error -> Errors
singleton = ErrorsOne

{- | Convert errors to builder. The errors are separated by
a pair of characters: comma and space.
-}
builderUtf8 :: Errors -> Builder
builderUtf8 errs =
  let len = countErrors errs
      errArr = makeErrorArray len errs
   in Error.builderUtf8 (PM.indexSmallArray errArr 0)
        <> Arr.foldMap
          (\e -> Builder.ascii2 ',' ' ' <> Error.builderUtf8 e)
          (Arr.slice errArr 1 (len - 1))

{- | Print errors to the provided handle. Typically, @System.IO.stderr@
is provided as the handle. Each encoded error is suffixed with a newline.

This is a convenience function for the common case where, after a
failed parse, an application prints out all parse errors and then exits.
-}
hPut :: Handle -> Errors -> IO ()
hPut h errs = do
  let len = countErrors errs
      errArr = makeErrorArray len errs
      bldr =
        Arr.foldMap
          (\e -> Error.builderUtf8 e <> Builder.ascii '\n')
          errArr
   in ByteChunks.hPut h (Builder.run 128 bldr)

-- | Convert errors to array.
toSmallArray :: Errors -> SmallArray Error
toSmallArray e = makeErrorArray (countErrors e) e

makeErrorArrayErrorThunk :: a
{-# NOINLINE makeErrorArrayErrorThunk #-}
makeErrorArrayErrorThunk =
  errorWithoutStackTrace "Json.Arrow.makeErrorArray: implementation mistake"

makeErrorArray :: Int -> Errors -> SmallArray Error
makeErrorArray !len errs0 = runST $ do
  dst <- PM.newSmallArray len makeErrorArrayErrorThunk
  let go !ix errs = case errs of
        ErrorsOne e -> do
          PM.writeSmallArray dst ix e
          pure (ix + 1)
        ErrorsPlus a b -> do
          ix' <- go ix a
          go ix' b
  !finalIx <- go 0 errs0
  if finalIx == len
    then PM.unsafeFreezeSmallArray dst
    else errorWithoutStackTrace "Json.Arrow.makeErrorArray: other impl mistake"

-- postcondition: results is greater than 0.
countErrors :: Errors -> Int
countErrors = go
 where
  go ErrorsOne {} = 1
  go (ErrorsPlus a b) = go a + go b

ba2st :: PM.ByteArray -> ShortText
{-# INLINE ba2st #-}
ba2st (PM.ByteArray x) = TS.fromShortByteStringUnsafe (SBS x)

encode :: Errors -> ShortText
encode p = ba2st (ByteChunks.concatU (Builder.run 128 (builderUtf8 p)))
