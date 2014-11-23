{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module Main (main) where

import Test.Tasty as Tasty
import Test.Tasty.HUnit as Tasty (testCase)
import Test.Tasty.QuickCheck as Tasty (testProperty)
import Test.Tasty.TH as Tasty
import Test.HUnit as HUnit hiding (State)
import Sajson
import qualified Sajson as Sajson

assumeSuccess (Right r) = r
assumeSuccess (Left e) = error $ "Unexpected error: " ++ (show e)

case_parse_error :: IO ()
case_parse_error = do
    let Left r = parse "aoeuaoeu"
    assertEqual "" (ParseError 1 1 "document root must be object or array") r

case_parse_success = do
    let r = parse "{}"
    case r of
        Right _ -> return ()
        Left _ -> assertFailure "Parsing should have succeeded"

case_get_value_type = do
    assertEqual "" TObject (typeOf $ root $ assumeSuccess $ parse "{}")
    assertEqual "" TArray (typeOf $ root $ assumeSuccess $ parse "[]")

case_get_length = do
    let Right r = parse "[1,2,3,4,5]"
    let Just a = asArray $ root r
    assertEqual "" 5 (Sajson.length a)

case_num_keys = do
    let Right r = parse "{\"hello\":\"world\",\"foo\":999}"
    let Just o = asObject $ root r
    assertEqual "" 2 (Sajson.numKeys o)

case_asString = do
    let Right r = parse "[\"Hello\"]"
    let Just a = asArray $ root r
    let s = asString $ getArrayElement a 0
    assertEqual "" (Just "Hello") s

case_get_element = do
    let Right r = parse "[1,2,3,4,6]"
    let Just a = asArray $ root r
    let len = Sajson.length a
    let last = getArrayElement a (len - 1)
    assertEqual "" (Just 6) (asInt last)

case_getObjectKey = do
    let Right r = parse "{\"foo\":\"bar\"}"
    let Just o = asObject $ root r
    let k = getObjectKey o 0
    assertEqual "" "foo" k

case_getObjectValue = do
    let Right r = parse "{\"foo\":\"bar\"}"
    let Just o = asObject $ root r
    let k = getObjectKey o 0
    let v = asString $ getObjectValue o 0
    assertEqual "" "foo" k
    assertEqual "" (Just "bar") v

tests :: TestTree
tests = $(testGroupGenerator)

main :: IO ()
main = Tasty.defaultMain tests
