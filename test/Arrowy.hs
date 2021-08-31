{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}

module Arrowy
  ( decode
  , expectationBad
  ) where

import Prelude hiding (id)

import Control.Arrow ((>>>),(<+>))
import DogHouse(Dog(..),House(..))
import Json.Arrow (type (~>),Members,Error(..),Context(..))

import qualified Json
import qualified Json.Arrow as P

decode :: Json.Value -> Either [Error] House
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

expectationBad :: Either [Error] House
expectationBad = Left
  [ Error "expected number" $ Key "age" $ Idx 1 $ Key "dogs" Top
  , Error "expected null" $ Key "age" $ Idx 1 $ Key "dogs" Top
  ]
