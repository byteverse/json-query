{-# language BangPatterns #-}
{-# language BlockArguments #-}
{-# language DeriveFunctor #-}
{-# language DerivingStrategies #-}
{-# language DuplicateRecordFields #-}
{-# language KindSignatures #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}
{-# language RankNTypes #-}
{-# language OverloadedStrings #-}

module Json.Parser
  ( Parser(..)
  , MemberParser(..)
    -- * Run
  , run
    -- * Object Parsing
  , key
  , keyOptNull
  , members
    -- * Arrays
  , smallArray
  , traverseMembers
    -- * Specific Data Constructors
  , object
  , array
  , number
  , boolean
  , string
    -- * Trivial Combinators
  , int
  , int32
  , word16
  , word64
    -- * Failing
  , fail
    -- * Modified Context 
  , contextually
  ) where

import Prelude hiding (fail)

import Control.Applicative (Alternative(..))
import Control.Monad.ST (runST)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT(ExceptT),runExceptT)
import Data.Foldable (foldlM)
import Data.Int (Int32)
import Data.List (find)
import Data.Number.Scientific (Scientific)
import Data.Primitive (SmallArray)
import Data.Text.Short (ShortText)
import Data.Word (Word16,Word64)
import Json (Value(Object,Array,Number),Member(Member))
-- import Json.Path (Path(Nil,Key,Index))
import Json.Context (Context(Top,Key,Index))
import Json.Errors (Errors)
import Json.Error (Error(..))

import qualified Data.Number.Scientific as SCI
import qualified Data.Primitive as PM
import qualified Json
import qualified Json.Errors as Errors

newtype Parser a = Parser
  { runParser :: Context -> Either Errors a }
  deriving stock Functor

instance Applicative Parser where
  pure a = Parser (\_ -> Right a)
  Parser f <*> Parser g = Parser $ \p -> do
    h <- f p
    y <- g p
    pure (h y)

instance Alternative Parser where
  empty = fail mempty
  f <|> g = Parser $ \p -> case runParser f p of
    Right x -> Right x
    Left aErrs -> case runParser g p of
      Right y -> Right y
      Left bErrs -> Left (aErrs <> bErrs)

instance Monad Parser where
  Parser f >>= g = Parser $ \p -> do
    x <- f p
    runParser (g x) p

newtype MemberParser a = MemberParser
  { runMemberParser :: Context -> SmallArray Member -> Either Errors a }
  deriving stock Functor

instance Applicative MemberParser where
  pure a = MemberParser (\_ _ -> Right a)
  MemberParser f <*> MemberParser g = MemberParser $ \p mbrs -> do
    h <- f p mbrs
    y <- g p mbrs
    pure (h y)

instance Alternative MemberParser where
  empty = MemberParser $ \p _ -> Left (Errors.singleton Error{message=mempty,context=p})
  a <|> b = MemberParser $ \p mbrs ->
    case runMemberParser a p mbrs of
      Right x -> Right x
      Left aErrs -> case runMemberParser b p mbrs of
        Right y -> Right y
        Left bErrs -> Left (aErrs <> bErrs)

instance Monad MemberParser where
  parser >>= k = MemberParser $ \p mbrs ->
    case runMemberParser parser p mbrs of
      Left p' -> Left p'
      Right x -> runMemberParser (k x) p mbrs

run :: Parser a -> Either Errors a
run (Parser f) = case f Top of
  Right a -> Right a
  Left e -> Left e

fail :: ShortText -> Parser a
fail !msg = Parser (\e -> Left (Errors.singleton Error{context=e,message=msg}))

object :: Value -> Parser (SmallArray Member)
object = \case
  Object xs -> pure xs
  _ -> fail "expected object"

array :: Value -> Parser (SmallArray Value)
array = \case
  Array xs -> pure xs
  _ -> fail "expected array"

members :: MemberParser a -> SmallArray Member -> Parser a
members (MemberParser f) mbrs = Parser (\p -> f p mbrs)

number :: Value -> Parser Scientific
number = \case
  Number n -> pure n
  _ -> fail "expected number"

string :: Value -> Parser ShortText
string = \case
  Json.String n -> pure n
  _ -> fail "expected string"

int :: Scientific -> Parser Int
int m = case SCI.toInt m of
  Just n -> pure n
  _ -> fail "expected number in signed machine integer range"

int32 :: Scientific -> Parser Int32
int32 m = case SCI.toInt32 m of
  Just n -> pure n
  _ -> fail "expected number in range [-2^31,2^31-1)"

word16 :: Scientific -> Parser Word16
word16 m = case SCI.toWord16 m of
  Just n -> pure n
  _ -> fail "expected number in range [0,2^16)"

word64 :: Scientific -> Parser Word64
word64 m = case SCI.toWord64 m of
  Just n -> pure n
  _ -> fail "expected number in range [0,2^64)"

boolean :: Value -> Parser Bool
boolean = \case
  Json.True -> pure True
  Json.False -> pure False
  _ -> fail "expected boolean"

-- members :: Parser Value (Chunks Member)
-- members = _

key :: ShortText -> (Value -> Parser a) -> MemberParser a
key !name f = MemberParser $ \p mbrs ->
  let !p' = Key name p in
  case find (\Member{key=k} -> k == name) mbrs of
    Nothing -> Left (Errors.singleton (Error{context=p',message="key not found: " <> name}))
    Just Member{value} -> runParser (f value) p'

-- | Variant of 'key' that supplies the JSON value @null to the
-- callback if the key is not found. Using this parser combinators implies
-- that there is no distinction between @null@ and an absent value in
-- the encoding scheme.
keyOptNull :: ShortText -> (Value -> Parser a) -> MemberParser a
keyOptNull !name f = MemberParser $ \p mbrs ->
  let !p' = Key name p
      val = case find (\Member{key=k} -> k == name) mbrs of
        Nothing -> Json.Null
        Just Member{value} -> value
   in runParser (f val) p'

-- object2 ::
--      (a -> b -> c)
--   -> ShortText -> Parser a
--   -> ShortText -> Parser b
--   -> Parser c
-- object2 f ka pa kb pb = Parser $ \p v -> case v 

-- elements :: Parser Value (Chunks Value)
-- elements = _

-- | Run the same parser against every element in a 'SmallArray'. This adjusts
-- the context at each element.
smallArray :: (Value -> Parser a) -> SmallArray Value -> Parser (SmallArray a)
smallArray f xs = Parser $ \ !p -> runST do
  let !len = length xs
  dst <- PM.newSmallArray len errorThunk
  runExceptT $ do
    _ <- foldlM
      (\ix x -> do
        !y <- ExceptT (pure (runParser (f x) (Index ix p)))
        lift (PM.writeSmallArray dst ix y)
        pure (ix + 1)
      ) 0 xs
    lift (PM.unsafeFreezeSmallArray dst)

-- | Traverse the members. The adjusts the context at each member.
traverseMembers :: (Member -> Parser a) -> SmallArray Member -> Parser (SmallArray a)
traverseMembers f !xs = Parser $ \ !p -> runST do
  let !len = length xs
  dst <- PM.newSmallArray len errorThunk
  runExceptT $ do
    !_ <- foldlM
      (\ !ix x@Member{key=k} -> do
        !y <- ExceptT (pure (runParser (f x) (Key k p)))
        lift (PM.writeSmallArray dst ix y)
        pure (ix + 1)
      ) 0 xs
    lift (PM.unsafeFreezeSmallArray dst)

errorThunk :: a
{-# noinline errorThunk #-}
errorThunk = errorWithoutStackTrace "Json.Parser: implementation mistake"

-- | Run a parser in a modified context.
contextually :: (Context -> Context) -> Parser a -> Parser a
{-# inline contextually #-}
contextually f (Parser g) = Parser
  (\p ->
    let !p' = f p
     in g p'
  )
