{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module Main (main) where

import Data.Maybe (isJust)
import Test.Tasty as Tasty
import Test.Tasty.HUnit as Tasty (testCase)
import Test.Tasty.QuickCheck as Tasty (testProperty)
import Test.Tasty.TH as Tasty
import Test.HUnit as HUnit hiding (State)
import Sajson
import qualified Sajson as Sajson

assumeSuccess (Right r) = r
assumeSuccess (Left e) = error $ "Unexpected error: " ++ (show e)

parseObject s =
    let Right r = parse s
        Just o = asObject $ root r
    in o

parseArray s =
    let Right r = parse s
        Just a = asArray $ root r
    in a

assertFalse message True = assertFailure message
assertFalse _message False = return ()

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

case_asDouble = do
    let a = parseArray "[3.14]"
    let v = getArrayElement a 0
    let d = asDouble v
    assertEqual "" (Just 3.14) d

case_getObjectKey = do
    let Right r = parse "{\"foo\":\"bar\"}"
    let Just o = asObject $ root r
    let k = getObjectKey o 0
    assertEqual "" "foo" k

case_getObjectValue = do
    let Right r = parse "{\"foo\":\"bar\"}"
    let Just o = asObject $ root r
    assertEqual "" 1 (numKeys o)
    let k = getObjectKey o 0
    let v = asString $ getObjectValue o 0
    assertEqual "" "foo" k
    assertEqual "" (Just "bar") v

case_indexOfObjectKey = do
    let o = parseObject "{\"foo\":\"bar\",\"baz\":999,\"quux\":null}"
    let Just index = indexOfObjectKey o "baz"
    let k = getObjectKey o index
    let v = getObjectValue o index
    let vi = asInt v
    assertEqual "" "baz" k
    assertEqual "" (Just 999) vi

case_getObjectWithKey = do
    let o = parseObject "{\"foo\":\"bar\",\"baz\":999,\"quux\":null}"
    let v = getObjectWithKey o "quux"
    assertEqual "" (Just TNull) (fmap typeOf v)

    assertFalse "" (isJust $ getObjectWithKey o "does not exist")

tests :: TestTree
tests = $(testGroupGenerator)

main :: IO ()
main = Tasty.defaultMain tests
