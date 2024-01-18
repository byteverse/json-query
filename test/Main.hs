{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Arrow ((>>>))
import Data.Bifunctor (first)
import Data.Bytes (Bytes)
import Data.Word (Word32, Word64)
import Json.Error (Error (Error))
import Json.Path (Path (Index, Key, Nil))
import Test.Hspec
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit ((@=?))
import Test.Tasty.Hspec

import qualified Data.Bytes.Text.Ascii as Ascii
import qualified Data.Number.Scientific as SCI
import qualified GHC.Exts as Exts
import qualified Json
import qualified Json.Arrow as A
import qualified Json.Context as Ctx
import qualified Json.Error as JE
import qualified Json.Errors as JES
import qualified Json.Parser as P
import qualified Json.Path as Path
import qualified Test.Tasty.HUnit as THU

import qualified Arrowy
import qualified DogHouse
import qualified Monadic

main :: IO ()
main = do
  hSpecGroup <- testSpec "Hspec Tests" hSpecTests
  defaultMain $ testGroup "All Tests" [hSpecGroup, hUnitTests]

hSpecTests :: Spec
hSpecTests =
  describe "Parser" $ do
    describe "word32" $ do
      context "when value is negative" $ do
        it "should fail" $ do
          let input = SCI.small (-1) 0
          let actual = P.runParser (P.word32 input) Ctx.Top
          let expected = Left (JES.singleton JE.Error {JE.message = "expected number in range [0,2^32)", JE.context = Ctx.Top})
          actual `shouldBe` expected

      context "when value is zero" $ do
        it "should succeed" $ do
          let input = SCI.small 0 0
          let actual = P.runParser (P.word32 input) Ctx.Top
          let expected = Right 0
          actual `shouldBe` expected

      context "when value is upper bound" $ do
        it "should succeed" $ do
          let input = SCI.fromWord32 (maxBound :: Word32)
          let actual = P.runParser (P.word32 input) Ctx.Top
          let expected = Right (maxBound :: Word32)
          actual `shouldBe` expected

      context "when value is greater than upper bound" $ do
        it "should fail" $ do
          let large :: Word64 = fromIntegral (maxBound :: Word32) + 1
          let input = SCI.fromWord64 large
          let actual = P.runParser (P.word32 input) Ctx.Top
          let expected = Left (JES.singleton JE.Error {JE.message = "expected number in range [0,2^32)", JE.context = Ctx.Top})
          actual `shouldBe` expected

hUnitTests :: TestTree
hUnitTests =
  testGroup
    "HUnit Tests"
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
            @=? first JES.toSmallArray (A.run (A.object >>> A.member "foo" >>> A.strings) val)
    , THU.testCase "Arrowy-encode-errors" $
        "$.dogs[1].age: expected number, $.dogs[1].age: expected null"
          @=? JES.encode Arrowy.badErrors
    ]

sampleArrowyStrings :: Bytes
sampleArrowyStrings =
  Ascii.fromString
    "{\"bla\": true, \"foo\" : [ \"bar\", \"baz\", null, \"hey\"] }"
