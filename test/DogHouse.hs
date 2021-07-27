{-# language ApplicativeDo #-}
{-# language NamedFieldPuns #-}
{-# language OverloadedStrings #-}
{-# language QuasiQuotes #-}

module DogHouse
  ( House(..)
  , Dog(..)
  , sampleGood
  , sampleBad
  , expectationGood
  ) where

import Data.ByteString.Short.Internal (ShortByteString(SBS))
import Data.Bytes (Bytes)
import Data.Primitive (ByteArray)
import Data.Primitive (SmallArray)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Short (ShortText)
import NeatInterpolation (text)

import qualified Data.ByteString.Short as SBS
import qualified Data.Bytes as Bytes
import qualified Data.Primitive as PM
import qualified GHC.Exts as Exts

data House = House
  { address :: !ShortText
  , dogs :: !(SmallArray Dog)
  } deriving (Eq,Show)

data Dog = Dog
  { name :: !ShortText
  , age :: !Int
  , alive :: !Bool
  } deriving (Eq,Show)

shortByteStringToByteArray :: ShortByteString -> ByteArray 
shortByteStringToByteArray (SBS x) = PM.ByteArray x

expectationGood :: House
expectationGood = House
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

sampleGood :: Bytes
sampleGood = id
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

sampleBad :: Bytes
sampleBad = id
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
