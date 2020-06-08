{-# language ApplicativeDo #-}
{-# language NamedFieldPuns #-}
{-# language OverloadedStrings #-}
{-# language QuasiQuotes #-}

module DogHouse
  ( House(..)
  , Dog(..)
  , decode
  , sampleA
  , sampleB
  , expectationA
  , expectationB
  ) where

import Control.Monad ((>=>))
import Data.ByteString.Short.Internal (ShortByteString(SBS))
import Data.Bytes (Bytes)
import Data.Primitive (ByteArray)
import Data.Primitive (SmallArray)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Short (ShortText)
import Json.Parser (Parser,MemberParser)
import Json.Path (Path(Key,Index,Nil))
import NeatInterpolation (text)

import qualified Data.ByteString.Short as SBS
import qualified Data.Bytes as Bytes
import qualified Data.Primitive as PM
import qualified GHC.Exts as Exts
import qualified Json
import qualified Json.Parser as P

data House = House
  { address :: !ShortText
  , dogs :: !(SmallArray Dog)
  } deriving (Eq,Show)

data Dog = Dog
  { name :: !ShortText
  , age :: !Int
  , alive :: !Bool
  } deriving (Eq,Show)

decode :: Json.Value -> Either Path House
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

shortByteStringToByteArray :: ShortByteString -> ByteArray 
shortByteStringToByteArray (SBS x) = PM.ByteArray x

expectationA :: Either Path House
expectationA = Right House
  { address = "123 Walsh Street"
  , dogs = Exts.fromList
    [ Dog
      { name = "Fluff"
      , age = 52
      , alive = True
      }
    , Dog
      { name = "McDeath"
      , age = 98
      , alive = False
      }
    ]
  }

expectationB :: Either Path House
expectationB = Left (Key "dogs" $ Index 1 $ Key "age" $ Nil)

sampleA :: Bytes
sampleA = id
  $ Bytes.fromByteArray
  $ shortByteStringToByteArray
  $ SBS.toShort
  $ encodeUtf8
  [text|
  {
    "address": "123 Walsh Street",
    "dogs": [
      { "name": "Fluff",
        "age": 52,
        "alive": true
      },
      { "name": "McDeath",
        "age": 98,
        "alive": false
      }
    ]
  }
  |]

sampleB :: Bytes
sampleB = id
  $ Bytes.fromByteArray
  $ shortByteStringToByteArray
  $ SBS.toShort
  $ encodeUtf8
  [text|
  {
    "address": "123 Walsh Street",
    "dogs": [
      { "name": "Fluff", "age": 52, "alive": true },
      { "name": "McDeath", "age": true, "alive": false }
    ]
  }
  |]
