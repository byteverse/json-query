{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Json.Arrow
  ( Parser
  , type (~>)
  , run
  , Error(..)
  , errorToUtf8
  , Context(..)
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
import Data.Bytes.Builder (Builder)
import Data.List (find)
import Data.Number.Scientific (Scientific)
import Data.Primitive (SmallArray)
import Data.Primitive.Unlifted.Array (UnliftedArray)
import Data.Profunctor (Profunctor(..))
import Data.Text.Short (ShortText)
import Data.Word (Word16,Word64)
import Json (Value(Object,Array,String,Number), Member(Member))

import qualified Data.Bytes.Builder as Builder
import qualified Data.Number.Scientific as SCI
import qualified Data.Primitive.Contiguous as Arr
import qualified Json


newtype Parser a b = P
  { unParser :: Context -- ^ reverse list of json keys & indices that have been entered
             -> a -- ^ value to parse
             -> Either [Error] (Context, b)
  }
type a ~> b = Parser a b

run :: (a ~> b) -> a -> Either [Error] b
run (P p) x = snd <$> p Top x

-- | keys and indexes through json stored with deepest key/index nearest the head ctor
data Context
  = Top
  | Key !ShortText !Context
  | Idx !Int !Context
  deriving (Eq,Show)

data Error = Error
  { message :: !ShortText
  , context :: !Context
  } deriving (Eq,Show)


object :: Value ~> Members
object = P $ \ctx v -> case v of
  Object membs -> Right (ctx, Members membs)
  _ -> Left [Error "expected object" ctx]

array :: Value ~> Elements
array = P $ \ctx v -> case v of
  Array membs -> Right (ctx, Elements membs)
  _ -> Left [Error "expected array" ctx]

string :: Value ~> ShortText
string = P $ \ctx v -> case v of
  String str -> Right (ctx, str)
  _ -> Left [Error "expected string" ctx]

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
        _ -> ExceptT (pure (Left [Error "expected string" (Idx ix ctx)]))
      ) membs
    pure (ctx, xs)
  _ -> Left [Error "expected array" ctx]

number :: Value ~> Scientific
number = P $ \ctx v -> case v of
  Number n -> Right (ctx, n)
  _ -> Left [Error "expected number" ctx]

boolean :: Value ~> Bool
boolean = P $ \ctx v -> case v of
  Json.True -> Right (ctx, True)
  Json.False -> Right (ctx, False)
  _  -> Left [Error "expected boolean" ctx]

null :: Value ~> ()
null = P $ \ctx v -> case v of
  Json.Null -> Right (ctx, ())
  _ -> Left [Error "expected null" ctx]

newtype Members = Members { unMembers :: SmallArray Member }

member :: ShortText -> Members ~> Value
member k = P $ \ctx xs -> case find keyEq (unMembers xs) of
  Just Member{value} -> Right (Key k ctx, value)
  Nothing -> Left [Error ("key not found: " <> k) ctx]
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
        then case unParser (f z) (Idx i ctx) (Arr.index xs i) of
          Right (_, z') -> loop z' (i + 1)
          Left err -> Left err
        else Right (ctx, z)
   in loop z0 0

map :: (Value ~> a) -> Elements ~> SmallArray a
map (P p) = P $ \ctx (Elements xs) -> runST $ do
  let !len = length xs
  dst <- Arr.new len
  let loop !i = if i < len
        then case p (Idx i ctx) (Arr.index xs i) of
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
      Left errRight -> Left $ errLeft ++ errRight

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
fail msg = P $ \ctx _ -> Left [Error msg ctx]

failZero :: a ~> b
failZero = P $ \ctx _ -> Left [Error "" ctx]

liftMaybe ::
     ShortText -- ^ Message to display on decode error
  -> (a -> Maybe b) -- ^ Decode function
  -> a ~> b
liftMaybe msg f = P $ \ctx x -> case f x of
  Just y -> Right (ctx, y)
  Nothing -> Left [Error msg ctx]

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

contextToUtf8 :: Context -> Builder
contextToUtf8 ctx0 = Builder.ascii '$' <> go ctx0
  where
  go Top = mempty
  go (Key k ctx) = go ctx <> Builder.ascii '.' <> Builder.shortTextUtf8 k
  go (Idx i ctx) =
       go ctx
    <> Builder.ascii '['
    <> Builder.wordDec (fromIntegral i)
    <> Builder.ascii ']'
