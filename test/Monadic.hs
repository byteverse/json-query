{-# language ApplicativeDo #-}
{-# language NamedFieldPuns #-}
{-# language OverloadedStrings #-}
{-# language QuasiQuotes #-}

module Monadic
  ( decode
  , expectationBad
  ) where

import Control.Monad ((>=>))
import DogHouse(Dog(..),House(..))
import Json.Parser (MemberParser,Multipath(..))
import Json.Path (Path(Key,Index,Nil))

import qualified Json
import qualified Json.Parser as P

decode :: Json.Value -> Either Multipath House
decode v = P.run (P.object v >>= P.members houseMemberParser)

houseMemberParser :: MemberParser House
houseMemberParser = do
  address <- P.key "address" P.string
  dogs <- P.key "dogs" $ \v -> do
    arr <- P.array v
    flip P.smallArray arr $ \e -> do
      P.object e >>= P.members dogMemberParser
  pure (House{address,dogs})

dogMemberParser :: MemberParser Dog
dogMemberParser = do
  name <- P.key "name" P.string
  age <- P.key "age" (P.number >=> P.int)
  alive <- P.key "alive" P.boolean
  pure Dog{name,age,alive}

expectationBad :: Either Multipath House
expectationBad = Left $ Multipath [Key "dogs" $ Index 1 $ Key "age" $ Nil]
