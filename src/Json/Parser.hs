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
  , Multipath(..)
    -- * Run
  , run
    -- * Object Parsing
  , key
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
import Data.List (find)
import Data.Number.Scientific (Scientific)
import Data.Primitive (SmallArray)
import Data.Text.Short (ShortText)
import Data.Word (Word16,Word64)
import Json (Value(Object,Array,Number),Member(Member))
import Json.Path (Path(Nil,Key,Index))

import qualified Data.Number.Scientific as SCI
import qualified Data.Primitive as PM
import qualified Json
import qualified Json.Path as Path

newtype Parser a = Parser
  { runParser :: Path -> Either Multipath a }
  deriving stock Functor

instance Applicative Parser where
  pure a = Parser (\_ -> Right a)
  Parser f <*> Parser g = Parser $ \p -> do
    h <- f p
    y <- g p
    pure (h y)

instance Alternative Parser where
  empty = fail
  f <|> g = Parser $ \p -> case runParser f p of
    Right x -> Right x
    Left (Multipath aErrs) -> case runParser g p of
      Right y -> Right y
      Left (Multipath bErrs) -> Left (Multipath $ aErrs ++ bErrs)

instance Monad Parser where
  Parser f >>= g = Parser $ \p -> do
    x <- f p
    runParser (g x) p

newtype MemberParser a = MemberParser
  { runMemberParser :: Path -> SmallArray Member -> Either Multipath a }
  deriving stock Functor

instance Applicative MemberParser where
  pure a = MemberParser (\_ _ -> Right a)
  MemberParser f <*> MemberParser g = MemberParser $ \p mbrs -> do
    h <- f p mbrs
    y <- g p mbrs
    pure (h y)

instance Alternative MemberParser where
  empty = MemberParser $ \p _ -> Left $ Multipath [p]
  a <|> b = MemberParser $ \p mbrs ->
    case runMemberParser a p mbrs of
      Right x -> Right x
      Left (Multipath aErrs) -> case runMemberParser b p mbrs of
        Right y -> Right y
        Left (Multipath bErrs) -> Left (Multipath $ aErrs ++ bErrs)

instance Monad MemberParser where
  parser >>= k = MemberParser $ \p mbrs ->
    case runMemberParser parser p mbrs of
      Left p' -> Left p'
      Right x -> runMemberParser (k x) p mbrs

run :: Parser a -> Either Multipath a
run (Parser f) = case f Nil of
  Right a -> Right a
  Left e -> Left (reverseMultipath e)

fail :: Parser a
fail = Parser (\e -> Left $ Multipath [e])

object :: Value -> Parser (SmallArray Member)
object = \case
  Object xs -> pure xs
  _ -> fail

array :: Value -> Parser (SmallArray Value)
array = \case
  Array xs -> pure xs
  _ -> fail

members :: MemberParser a -> SmallArray Member -> Parser a
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

word64 :: Scientific -> Parser Word64
word64 m = case SCI.toWord64 m of
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
    Nothing -> Left $ Multipath [p']
    Just Member{value} -> runParser (f value) p'

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
contextually :: (Path -> Path) -> Parser a -> Parser a
{-# inline contextually #-}
contextually f (Parser g) = Parser
  (\p ->
    let !p' = f p
     in g p'
  )


newtype Multipath = Multipath { getMultipath :: [Path] }
  deriving (Show, Eq)

reverseMultipath :: Multipath -> Multipath
reverseMultipath (Multipath ps) = Multipath (Path.reverse <$> ps)
