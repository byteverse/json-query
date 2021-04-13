{-# language BangPatterns #-}
{-# language MagicHash #-}
{-# language UnboxedTuples #-}

module Json.Path
  ( Path(..)
    -- * Encoding
  , encode
  , builderUtf8
    -- * Lookup
  , query
    -- * Reverse
  , reverse
  ) where

import Prelude hiding (reverse)

import Json (Value(Object,Array),Member(Member))
import Data.Primitive (ByteArray(ByteArray))
import Data.Text.Short (ShortText)
import Data.Bytes.Builder (Builder)
import Data.ByteString.Short.Internal (ShortByteString(SBS))

import qualified Data.Bytes.Chunks as ByteChunks
import qualified Data.Bytes.Builder as Builder
import qualified Data.Primitive as PM
import qualified Data.Text.Short.Unsafe as TS

-- | A path to an object.
data Path
  = Key {-# UNPACK #-} !ShortText !Path
    -- ^ JSON path element of a key into an object, \"object.key\".
  | Index {-# UNPACK #-} !Int !Path
    -- ^ JSON path element of an index into an array, \"array[index]\".
    -- Negative numbers result in undefined behavior.
  | Nil
  deriving (Eq,Show)

-- | Encode a path.
--
-- >>> encode (Key "foo" $ Index 5 $ Key "bar" $ Nil)
-- $.foo[5].bar
encode :: Path -> ShortText
encode p = ba2st (ByteChunks.concatU (Builder.run 128 (builderUtf8 p)))

builderUtf8 :: Path -> Builder
builderUtf8 p0 = Builder.ascii '$' <> go p0 where
  go Nil = mempty
  go (Key k p) = Builder.ascii '.' <> Builder.shortTextUtf8 k <> go p
  go (Index i p) =
       Builder.ascii '['
    <> Builder.wordDec (fromIntegral i)
    <> Builder.ascii ']'
    <> go p

-- | Search for an element at the given path.
query :: Path -> Value -> Maybe Value
query = go where
  go Nil v = Just v
  go (Key k p) (Object mbrs) = foldr
    (\(Member key val) other -> if key == k
      then Just val
      else other
    ) Nothing mbrs >>= go p
  go (Index i p) (Array vs) = if i < PM.sizeofSmallArray vs
    then
      let !(# e #) = PM.indexSmallArray## vs i
       in go p e
    else Nothing
  go _ _ = Nothing

ba2st :: ByteArray -> ShortText
ba2st (ByteArray x) = TS.fromShortByteStringUnsafe (SBS x)

reverse :: Path -> Path
reverse = go Nil where
  go !acc Nil = acc
  go !acc (Key k xs) = go (Key k acc) xs
  go !acc (Index i xs) = go (Index i acc) xs

instance Ord Path where
  compare a b = compare (len 0 a) (len 0 b)
    where
    len !acc (Key _ p) = len (acc + 1) p
    len !acc (Index _ p) = len (acc + 1) p
    len !acc Nil = acc
