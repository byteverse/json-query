{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Arrow ((>>>))
import Data.Bifunctor (first)
import Data.Bytes (Bytes)
import Json.Error (Error (Error))
import Json.Path (Path (Index, Key, Nil))
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit ((@=?))

import qualified Data.Bytes.Text.Ascii as Ascii
import qualified GHC.Exts as Exts
import qualified Json
import qualified Json.Arrow as A
import qualified Json.Context as Ctx
import qualified Json.Errors as Errors
import qualified Json.Path as Path
import qualified Test.Tasty.HUnit as THU

import qualified Arrowy
import qualified DogHouse
import qualified Json.Error
import qualified Monadic

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ THU.testCase "Json.Path.query-A" $
        case Json.decode (Ascii.fromString "{\"bla\": true, \"foo\" : [ 55 , { \"bar\": false } ] }") of
          Left _ -> fail "failed to parse into syntax tree"
          Right v ->
            Just Json.False
              @=? Path.query (Key "foo" $ Index 1 $ Key "bar" Nil) v
    , THU.testCase "Json.Path.encode-A" $
        "$.foo[1].bar"
          @=? Path.encode (Key "foo" $ Index 1 $ Key "bar" Nil)
    , THU.testCase "DogHouse-Monadic-A" $ case Json.decode DogHouse.sampleGood of
        Left _ -> fail "failed to parse into syntax tree"
        Right val ->
          Right DogHouse.expectationGood
            @=? Monadic.decode val
    , THU.testCase "DogHouse-Monadic-B" $ case Json.decode DogHouse.sampleBad of
        Left _ -> fail "failed to parse into syntax tree"
        Right val ->
          Monadic.expectationBad
            @=? Monadic.decode val
    , THU.testCase "DogHouse-Arrowy-A" $ case Json.decode DogHouse.sampleGood of
        Left _ -> fail "failed to parse into syntax tree"
        Right val ->
          Right DogHouse.expectationGood
            @=? Arrowy.decode val
    , THU.testCase "DogHouse-Arrowy-B" $ case Json.decode DogHouse.sampleBad of
        Left _ -> fail "failed to parse into syntax tree"
        Right val ->
          Left Arrowy.badErrors
            @=? Arrowy.decode val
    , THU.testCase "Arrowy-strings-A" $ case Json.decode sampleArrowyStrings of
        Left _ -> fail "failed to parse into syntax tree"
        Right val ->
          Left
            ( Exts.fromList
                [ Error
                    { context = Ctx.Index 2 (Ctx.Key "foo" Ctx.Top)
                    , message = "expected string"
                    }
                ]
            )
            @=? first Errors.toSmallArray (A.run (A.object >>> A.member "foo" >>> A.strings) val)
    , THU.testCase "Arrowy-encode-errors" $
        "$.dogs[1].age: expected number, $.dogs[1].age: expected null"
          @=? Errors.encode Arrowy.badErrors
    ]

sampleArrowyStrings :: Bytes
sampleArrowyStrings =
  Ascii.fromString
    "{\"bla\": true, \"foo\" : [ \"bar\", \"baz\", null, \"hey\"] }"
