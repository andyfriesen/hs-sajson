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
    assertEqual "" TObject $ typeOf $ root r
    let Just o = asObject $ root r
    let k = getObjectKey o 0
    assertEqual "" "bar" k
    return ()

-- case_walk = do
--     let Right r = parse "{\"foo\":\"bar\",\"baz\":999}"
--     let Just o = asObject $ root r
--     let [(k1, v1), (k2, v2)] = [(getObjectKey o i, getObjectValue o i) | i <- [0..1]]
--     let s1 = asString v1
--     let i2 = asInt v2
--     assertEqual "" ("foo", Just "bar") (k1, s1)
--     assertEqual "" ("baz", Just 999) (k2, i2)

tests :: TestTree
tests = $(testGroupGenerator)

main :: IO ()
main = Tasty.defaultMain tests
