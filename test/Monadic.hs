{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Monadic
  ( decode
  , expectationBad
  ) where

import Control.Monad ((>=>))
import DogHouse (Dog (..), House (..))
import Json.Context (Context (Index, Key, Top))
import Json.Error (Error (..))
import Json.Errors (Errors)
import Json.Parser (MemberParser)

import qualified Json
import qualified Json.Errors as Errors
import qualified Json.Parser as P

decode :: Json.Value -> Either Errors House
decode v = P.run (P.object v >>= P.members houseMemberParser)

houseMemberParser :: MemberParser House
houseMemberParser = do
  address <- P.key "address" P.string
  dogs <- P.key "dogs" $ \v -> do
    arr <- P.array v
    flip P.smallArray arr $ \e -> do
      P.object e >>= P.members dogMemberParser
  pure (House {address, dogs})

dogMemberParser :: MemberParser Dog
dogMemberParser = do
  name <- P.key "name" P.string
  age <- P.key "age" (P.number >=> P.int)
  alive <- P.key "alive" P.boolean
  pure Dog {name, age, alive}

expectationBad :: Either Errors House
expectationBad =
  Left $
    Errors.singleton $
      Error
        { context = Key "age" $ Index 1 $ Key "dogs" $ Top
        , message = "expected number"
        }
