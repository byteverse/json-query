{-# language BangPatterns #-}
{-# language BlockArguments #-}
{-# language DeriveFunctor #-}
{-# language DerivingStrategies #-}
{-# language DuplicateRecordFields #-}
{-# language KindSignatures #-}
{-# language LambdaCase #-}
{-# language NamedFieldPuns #-}
{-# language RankNTypes #-}

module Json.Parser
  ( Parser(..)
  , MemberParser(..)
    -- * Run
  , run
    -- * Object Parsing
  , key
  , members
    -- * Arrays
  , smallArray
    -- * Specific Data Constructors
  , object
  , array
  , number
  , boolean
  , string
    -- * Trivial Combinators
  , int
  , word16
    -- * Failing
  , fail
  ) where

import Prelude hiding (fail)

import Control.Monad.ST (runST)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT(ExceptT),runExceptT)
import Data.Chunks (Chunks)
import Data.Foldable (foldlM)
import Data.List (find)
import Data.Kind (Type)
import Data.Primitive (SmallArray)
import Data.Text.Short (ShortText)
import Data.Word (Word16)
import Json (Value(Object,Array,Number),Member(Member))
import Json.Path (Path(Nil,Key,Index))
import Data.Number.Scientific (Scientific)

import qualified Data.Number.Scientific as SCI
import qualified Data.Primitive as PM
import qualified Json
import qualified Json.Path as Path

newtype Parser a = Parser
  { runParser :: Path -> Either Path a }
  deriving stock Functor

instance Applicative Parser where
  pure a = Parser (\_ -> Right a)
  Parser f <*> Parser g = Parser $ \p -> do
    h <- f p
    y <- g p
    pure (h y)

instance Monad Parser where
  Parser f >>= g = Parser $ \p -> do
    x <- f p
    runParser (g x) p

newtype MemberParser a = MemberParser
  { runMemberParser :: Path -> Chunks Member -> Either Path a }
  deriving stock Functor

instance Applicative MemberParser where
  pure a = MemberParser (\_ _ -> Right a)
  MemberParser f <*> MemberParser g = MemberParser $ \p mbrs -> do
    h <- f p mbrs
    y <- g p mbrs
    pure (h y)

run :: Parser a -> Either Path a
run (Parser f) = case f Nil of
  Right a -> Right a
  Left e -> Left (Path.reverse e)

fail :: Parser a
fail = Parser (\e -> Left e)

object :: Value -> Parser (Chunks Member)
object = \case
  Object xs -> pure xs
  _ -> fail

array :: Value -> Parser (Chunks Value)
array = \case
  Array xs -> pure xs
  _ -> fail

members :: MemberParser a -> Chunks Member -> Parser a
members (MemberParser f) mbrs = Parser (\p -> f p mbrs)

number :: Value -> Parser Scientific
number = \case
  Number n -> pure n
  _ -> fail

string :: Value -> Parser ShortText
string = \case
  Json.String n -> pure n
  _ -> fail

int :: Scientific -> Parser Int
int m = case SCI.toInt m of
  Just n -> pure n
  _ -> fail

word16 :: Scientific -> Parser Word16
word16 m = case SCI.toWord16 m of
  Just n -> pure n
  _ -> fail

boolean :: Value -> Parser Bool
boolean = \case
  Json.True -> pure True
  Json.False -> pure False
  _ -> fail

-- members :: Parser Value (Chunks Member)
-- members = _

key :: ShortText -> (Value -> Parser a) -> MemberParser a
key !name f = MemberParser $ \p mbrs ->
  let !p' = Key name p in
  case find (\Member{key=k} -> k == name) mbrs of
    Nothing -> Left p'
    Just Member{value} -> runParser (f value) p'

-- object2 ::
--      (a -> b -> c)
--   -> ShortText -> Parser a
--   -> ShortText -> Parser b
--   -> Parser c
-- object2 f ka pa kb pb = Parser $ \p v -> case v 

-- elements :: Parser Value (Chunks Value)
-- elements = _

smallArray :: (Value -> Parser a) -> Chunks Value -> Parser (SmallArray a)
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

errorThunk :: a
{-# noinline errorThunk #-}
errorThunk = errorWithoutStackTrace "Json.Parser: implementation mistake"
