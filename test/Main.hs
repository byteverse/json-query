{-# language LambdaCase #-}
{-# language OverloadedStrings #-}

import Data.Bytes (Bytes)
import Json.Path (Path(Key,Index,Nil))
import Test.Tasty (defaultMain,testGroup,TestTree)
import Test.Tasty.HUnit ((@=?))
import Control.Arrow ((>>>))

import qualified Data.Bytes as Bytes
import qualified Json
import qualified Json.Path as Path
import qualified Json.Arrow as A
import qualified Test.Tasty.HUnit as THU

import qualified Arrowy
import qualified DogHouse
import qualified Monadic

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ THU.testCase "Json.Path.query-A" $
      case Json.decode (Bytes.fromAsciiString "{\"bla\": true, \"foo\" : [ 55 , { \"bar\": false } ] }") of
        Left _ -> fail "failed to parse into syntax tree" 
        Right v -> 
          Just Json.False
          @=?
          Path.query (Key "foo" $ Index 1 $ Key "bar" $ Nil) v
  , THU.testCase "Json.Path.encode-A" $
      "$.foo[1].bar"
      @=?
      Path.encode (Key "foo" $ Index 1 $ Key "bar" $ Nil)
  , THU.testCase "DogHouse-Monadic-A" $ case Json.decode DogHouse.sampleGood of
      Left _ -> fail "failed to parse into syntax tree" 
      Right val ->
        Right DogHouse.expectationGood
        @=?
        Monadic.decode val
  , THU.testCase "DogHouse-Monadic-B" $ case Json.decode DogHouse.sampleBad of
      Left _ -> fail "failed to parse into syntax tree" 
      Right val ->
        Monadic.expectationBad
        @=?
        Monadic.decode val
  , THU.testCase "DogHouse-Arrowy-A" $ case Json.decode DogHouse.sampleGood of
      Left _ -> fail "failed to parse into syntax tree" 
      Right val ->
        Right DogHouse.expectationGood
        @=?
        Arrowy.decode val
  , THU.testCase "DogHouse-Arrowy-B" $ case Json.decode DogHouse.sampleBad of
      Left _ -> fail "failed to parse into syntax tree" 
      Right val ->
        Arrowy.expectationBad
        @=?
        Arrowy.decode val
  , THU.testCase "Arrowy-strings-A" $ case Json.decode sampleArrowyStrings of
      Left _ -> fail "failed to parse into syntax tree" 
      Right val ->
        Left
          ( A.ErrorsOne A.Error
            { A.context = A.Index 2 (A.Key "foo" A.Top)
            , A.message = "expected string"
            }
          )
        @=?
        A.run (A.object >>> A.member "foo" >>> A.strings) val
  , THU.testCase "Arrowy-encode-errors" $
      "$.dogs[1].age: expected number, $.dogs[1].age: expected null"
      @=?
      A.encodeErrors Arrowy.badErrors
  ]

sampleArrowyStrings :: Bytes
sampleArrowyStrings = Bytes.fromAsciiString
  "{\"bla\": true, \"foo\" : [ \"bar\", \"baz\", null, \"hey\"] }"
