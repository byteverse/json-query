{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Json.Arrow
  ( Parser
  , type (~>)
  , Error(..)
  , Errors(..)
  , Context(..)
  -- * Display Errors
  , errorToUtf8
  , errorsToUtf8
  , encodeErrors
  -- * Run Parser
  , run
  -- * Primitive Parsers
  , object
  , array
  , string
  , strings
  , number
  , boolean
  , null
  -- ** Object Members
  , Members
  , member
  , memberOpt
  , foldMembers
  -- ** Array Elements
  , Elements
  , foldl'
  , map
  -- * Primitive Combinators
  , fail
  , failZero
  -- * Trivial Combinators
  , withObject
  , withArray
  , fromNull
  , int
  , word16
  , word64
  -- * Conversion
  , liftMaybe
  ) where

import Prelude hiding (id, (.), fail, map, null)

import Control.Arrow ((>>>))
import Control.Arrow (Arrow(..))
import Control.Arrow (ArrowZero(..),ArrowPlus(..),ArrowChoice(..),ArrowApply(..))
import Control.Category (Category(..))
import Control.Monad.ST (runST)
import Control.Monad.Trans.Except (ExceptT(ExceptT),runExceptT)
import Data.ByteString.Short.Internal (ShortByteString(SBS))
import Data.Bytes.Builder (Builder)
import Data.List (find)
import Data.Number.Scientific (Scientific)
import Data.Primitive (SmallArray)
import Data.Primitive.Unlifted.Array (UnliftedArray)
import Data.Profunctor (Profunctor(..))
import Data.Text.Short (ShortText)
import Data.Word (Word16,Word64)
import Json (Value(Object,Array,String,Number), Member(Member))
import Json.Context (Context(..))

import qualified Data.Bytes.Chunks as ByteChunks
import qualified Data.Bytes.Builder as Builder
import qualified Data.Number.Scientific as SCI
import qualified Data.Primitive.Contiguous as Arr
import qualified Data.Primitive as PM
import qualified Json
import qualified Json.Context as Context
import qualified Json.Path as Path
import qualified Data.Text.Short.Unsafe as TS

newtype Parser a b = P
  { unParser :: Context -- ^ reverse list of json keys & indices that have been entered
             -> a -- ^ value to parse
             -> Either Errors (Context, b)
  }
type a ~> b = Parser a b

run :: (a ~> b) -> a -> Either Errors b
run (P p) x = snd <$> p Top x

data Error = Error
  { message :: !ShortText
  , context :: !Context
  } deriving (Eq,Show)

data Errors
  = ErrorsOne !Error
  | ErrorsPlus !Errors !Errors
  deriving (Eq,Show)

object :: Value ~> Members
object = P $ \ctx v -> case v of
  Object membs -> Right (ctx, Members membs)
  _ -> Left (ErrorsOne (Error "expected object" ctx))

array :: Value ~> Elements
array = P $ \ctx v -> case v of
  Array membs -> Right (ctx, Elements membs)
  _ -> Left (ErrorsOne (Error "expected array" ctx))

string :: Value ~> ShortText
string = P $ \ctx v -> case v of
  String str -> Right (ctx, str)
  _ -> Left (ErrorsOne (Error "expected string" ctx))

-- | Parse an array of strings. For example:
--
-- > ["hello","world"]
--
-- Failure context includes the index of non-string value if any values in
-- the array are not strings.
strings :: Value ~> UnliftedArray ShortText
strings = P $ \ctx v -> case v of
  Array membs -> runST $ runExceptT $ do
    xs <- Arr.itraverseP
      (\ix e -> case e of
        String s -> pure s
        _ -> ExceptT (pure (Left (ErrorsOne (Error "expected string" (Index ix ctx)))))
      ) membs
    pure (ctx, xs)
  _ -> Left (ErrorsOne (Error "expected array" ctx))

number :: Value ~> Scientific
number = P $ \ctx v -> case v of
  Number n -> Right (ctx, n)
  _ -> Left (ErrorsOne (Error "expected number" ctx))

boolean :: Value ~> Bool
boolean = P $ \ctx v -> case v of
  Json.True -> Right (ctx, True)
  Json.False -> Right (ctx, False)
  _  -> Left (ErrorsOne (Error "expected boolean" ctx))

null :: Value ~> ()
null = P $ \ctx v -> case v of
  Json.Null -> Right (ctx, ())
  _ -> Left (ErrorsOne (Error "expected null" ctx))

newtype Members = Members { unMembers :: SmallArray Member }

member :: ShortText -> Members ~> Value
member k = P $ \ctx xs -> case find keyEq (unMembers xs) of
  Just Member{value} -> Right (Key k ctx, value)
  Nothing -> Left (ErrorsOne (Error ("key not found: " <> k) ctx))
  where
  keyEq Member{key} = k == key

-- | An optional member. Returns Nothing if the value is missing.
memberOpt :: ShortText -> Members ~> Maybe Value
memberOpt k = P $ \ctx xs -> case find keyEq (unMembers xs) of
  Just Member{value} -> Right (Key k ctx, Just value)
  Nothing -> Right (ctx, Nothing)
  where
  keyEq Member{key} = k == key

foldMembers :: a -> (a -> Member ~> a) -> Members ~> a
foldMembers z0 f = P $ \ctx membs ->
  let xs = unMembers membs
      loop !z !i =
        if i < Arr.size xs
        then
          let x@Member{key} = Arr.index xs i
           in case unParser (f z) (Key key ctx) x of
                Right (_, z') -> loop z' (i + 1)
                Left err -> Left err
        else Right (ctx, z)
   in loop z0 0

newtype Elements = Elements { unElements :: SmallArray Value }

foldl' :: a -> (a -> Value ~> a) -> Elements ~> a
foldl' z0 f = P $ \ctx elems ->
  let xs = unElements elems
      loop !z !i =
        if i < Arr.size xs
        then case unParser (f z) (Index i ctx) (Arr.index xs i) of
          Right (_, z') -> loop z' (i + 1)
          Left err -> Left err
        else Right (ctx, z)
   in loop z0 0

map :: (Value ~> a) -> Elements ~> SmallArray a
map (P p) = P $ \ctx (Elements xs) -> runST $ do
  let !len = length xs
  dst <- Arr.new len
  let loop !i = if i < len
        then case p (Index i ctx) (Arr.index xs i) of
          Right (_, y) -> do
            Arr.write dst i y
            loop (i + 1)
          Left err -> pure $ Left err
        else pure $ Right ()
  loop 0 >>= \case
    Right _ -> do
      ys <- Arr.unsafeFreeze dst
      pure $ Right (ctx, ys)
    Left err -> pure $ Left err

instance Functor (Parser a) where
  fmap f (P p) = P $ \ctx x -> case p ctx x of
    Right (ctx', y) -> Right (ctx', f y)
    Left err -> Left err

instance Profunctor Parser where
  dimap g f (P p) = P $ \ctx x -> case p ctx (g x) of
    Right (ctx', y) -> Right (ctx', f y)
    Left err -> Left err

instance Applicative (Parser a) where
  pure x = P $ \ctx _ -> Right (ctx, x)
  (P p) <*> (P q) = P $ \ctx x -> case (p ctx x, q ctx x) of
    (Right (_, f), Right (_, y)) -> Right (ctx, f y)
    (Left err, _) -> Left err
    (_, Left err) -> Left err

instance Category Parser where
  id = P $ \ctx x -> Right (ctx, x)
  (P q) . (P p) = P $ \ctx x -> case p ctx x of
    Right (ctx', y) -> q ctx' y
    Left err -> Left err

instance Arrow Parser where
  arr f = P $ \ctx x -> Right (ctx, f x)
  (P p) *** (P q) = P $ \ctx (x, y) -> case (p ctx x, q ctx y) of
    (Right (_, x'), Right (_, y')) -> Right (ctx, (x', y'))
    (Left err, _) -> Left err
    (_, Left err) -> Left err

instance ArrowZero Parser where
  zeroArrow = failZero

instance ArrowPlus Parser where
  (P p) <+> (P q) = P $ \ctx x -> case p ctx x of
    Right success -> Right success
    Left errLeft -> case q ctx x of
      Right success -> Right success
      Left errRight -> Left $! ErrorsPlus errLeft errRight

instance ArrowChoice Parser where
  (P p) +++ (P q) = P $ \ctx -> \case
    Left x -> case p ctx x of
      Right (ctx', y) -> Right (ctx', Left y)
      Left err -> Left err
    Right x -> case q ctx x of
      Right (ctx', y) -> Right (ctx', Right y)
      Left err -> Left err

instance ArrowApply Parser where
  app = P $ \ctx (p, x) -> unParser p ctx x

fail :: ShortText -> a ~> b
fail msg = P $ \ctx _ -> Left (ErrorsOne (Error msg ctx))

failZero :: a ~> b
failZero = P $ \ctx _ -> Left (ErrorsOne (Error "" ctx))

liftMaybe ::
     ShortText -- ^ Message to display on decode error
  -> (a -> Maybe b) -- ^ Decode function
  -> a ~> b
liftMaybe msg f = P $ \ctx x -> case f x of
  Just y -> Right (ctx, y)
  Nothing -> Left (ErrorsOne (Error msg ctx))

withObject :: (Members ~> a) -> Value ~> a
withObject membParser = object >>> membParser

withArray :: (Value ~> a) -> Value ~> SmallArray a
withArray elemParser = array >>> map elemParser

int :: Value ~> Int
int = number >>> liftMaybe "number too big" SCI.toInt

word16 :: Value ~> Word16
word16 = number >>> liftMaybe "number too big" SCI.toWord16

word64 :: Value ~> Word64
word64 = number >>> liftMaybe "number too big" SCI.toWord64

fromNull :: a -> Value ~> a
fromNull z = null >>> pure z

errorToUtf8 :: Error -> Builder
errorToUtf8 (Error msg ctx)
  =  contextToUtf8 ctx
  <> Builder.ascii2 ':' ' '
  <> Builder.shortTextUtf8 msg

encodeErrors :: Errors -> ShortText
encodeErrors p =
  ba2st (ByteChunks.concatU (Builder.run 128 (errorsToUtf8 p)))

errorsToUtf8 :: Errors -> Builder
errorsToUtf8 errs =
  let len = countErrors errs
      errArr = makeErrorArray len errs
   in errorToUtf8 (PM.indexSmallArray errArr 0)
      <>
      Arr.foldMap
        (\e -> Builder.ascii2 ',' ' ' <> errorToUtf8 e)
        (Arr.slice errArr 1 (len - 1))

makeErrorArrayErrorThunk :: a
{-# noinline makeErrorArrayErrorThunk #-}
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
countErrors = go where
  go ErrorsOne{} = 1
  go (ErrorsPlus a b) = go a + go b

contextToUtf8 :: Context -> Builder
contextToUtf8 ctx0 = Path.builderUtf8 (Context.toPath ctx0)

ba2st :: PM.ByteArray -> ShortText
{-# inline ba2st #-}
ba2st (PM.ByteArray x) = TS.fromShortByteStringUnsafe (SBS x)
