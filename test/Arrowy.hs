{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}

module Arrowy
  ( decode
  , expectationBad
  , badErrors
  ) where

import Prelude hiding (id)

import Control.Arrow ((>>>),(<+>))
import DogHouse(Dog(..),House(..))
import Json.Arrow (type (~>),Members,Error(..),Context(..))
import Json.Arrow (Errors(..))

import qualified Json
import qualified Json.Arrow as P

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

expectationBad :: Either Errors House
expectationBad = Left badErrors

badErrors :: Errors
badErrors = ErrorsPlus
  (ErrorsOne (Error "expected number" $ Key "age" $ Index 1 $ Key "dogs" Top))
  (ErrorsOne (Error "expected null" $ Key "age" $ Index 1 $ Key "dogs" Top))
