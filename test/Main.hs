{-# language LambdaCase #-}
{-# language OverloadedStrings #-}

import Json.Path (Path(Key,Index,Nil))
import Test.Tasty (defaultMain,testGroup,TestTree)
import Test.Tasty.HUnit ((@=?))

import qualified Data.Bytes as Bytes
import qualified Json
import qualified Json.Path as Path
import qualified Test.Tasty.HUnit as THU

import qualified DogHouse

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
  , THU.testCase "DogHouse-A" $ case Json.decode DogHouse.sampleA of
      Left _ -> fail "failed to parse into syntax tree" 
      Right val ->
        DogHouse.expectationA
        @=?
        DogHouse.decode val
  , THU.testCase "DogHouse-B" $ case Json.decode DogHouse.sampleB of
      Left _ -> fail "failed to parse into syntax tree" 
      Right val ->
        DogHouse.expectationB
        @=?
        DogHouse.decode val
  ]
