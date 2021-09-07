{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}

module Arrowy
  ( decode
  , badErrors
  ) where

import Prelude hiding (id)

import Control.Arrow ((>>>),(<+>))
import DogHouse(Dog(..),House(..))
import Json.Arrow (type (~>),Members)
import Json.Errors (Errors)
import Json.Error (Error(..))
import Json.Context (Context(Key,Index,Top))
import Data.Primitive (SmallArray)

import qualified GHC.Exts as Exts
import qualified Json
import qualified Json.Arrow as P
import qualified Json.Errors as Errors

decode :: Json.Value -> Either Errors House
decode = P.run (P.object >>> houseMemberParser)

houseMemberParser :: Members ~> House
houseMemberParser = do
  address <- P.member "address" >>> P.string
  dogs <- P.member "dogs" >>> P.withArray (P.withObject dogMemberParser)
  pure House{address,dogs}

dogMemberParser :: Members ~> Dog
dogMemberParser = do
  name <- P.member "name" >>> P.string
  age <- P.member "age" >>> (P.int <+> P.fromNull 0)
  alive <- P.member "alive" >>> P.boolean
  pure Dog{name,age,alive}

badErrors :: Errors
badErrors =
  Errors.singleton (Error "expected number" $ Key "age" $ Index 1 $ Key "dogs" Top)
  <>
  Errors.singleton (Error "expected null" $ Key "age" $ Index 1 $ Key "dogs" Top)
