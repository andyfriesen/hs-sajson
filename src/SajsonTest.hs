{-# LANGUAGE TemplateHaskell, OverloadedStrings, RecordWildCards #-}

module Main (main) where

import Data.Maybe (isJust)
import Test.Tasty as Tasty
import Test.Tasty.HUnit as Tasty (testCase)
import Test.Tasty.QuickCheck as Tasty (testProperty)
import Test.Tasty.TH as Tasty
import Test.HUnit as HUnit hiding (State)
import Sajson
import qualified Sajson as Sajson

import Sajson.FromJson (FromJson (..), getKey, withObject)
import Data.Text (Text)

assumeSuccess (Right r) = r
assumeSuccess (Left e) = error $ "Unexpected error: " ++ (show e)

parseObject s =
    let Right r = parse s
        Just o = asObject r
    in o

parseArray s =
    let Right r = parse s
        Just a = asArray r
    in a

assertNotEqual a b
    | a == b = assertFailure $ "assertNotEqual failed: " ++ show a
    | otherwise = return ()

case_parse_error :: IO ()
case_parse_error = do
    let Left r = parse "aoeuaoeu"
    assertEqual "" (ParseError 1 1 "document root must be object or array") r

case_parse_success :: IO ()
case_parse_success = do
    let r = parse "{}"
    case r of
        Right _ -> return ()
        Left _ -> assertFailure "Parsing should have succeeded"

case_get_value_type :: IO ()
case_get_value_type = do
    assertEqual "" TObject (typeOf $ assumeSuccess $ parse "{}")
    assertEqual "" TArray (typeOf $ assumeSuccess $ parse "[]")

case_get_length :: IO ()
case_get_length = do
    let a = parseArray "[1,2,3,4,5]"
    assertEqual "" 5 (Sajson.length a)

case_num_keys :: IO ()
case_num_keys = do
    let o = parseObject "{\"hello\":\"world\",\"foo\":999}"
    assertEqual "" 2 (Sajson.numKeys o)

case_asString :: IO ()
case_asString = do
    let a = parseArray "[\"Hello\"]"
    let s = asString $ getArrayElement a 0
    assertEqual "" (Just "Hello") s

case_get_element :: IO ()
case_get_element = do
    let a = parseArray "[1,2,3,4,6]"
    let len = Sajson.length a
    let last = getArrayElement a (len - 1)
    assertEqual "" (Just 6) (asInt last)

case_asInt :: IO ()
case_asInt = do
    let a = parseArray "[3.14,5]"
    let v1 = getArrayElement a 0
    let v2 = getArrayElement a 1
    let d1 = asInt v1
    let d2 = asInt v2
    assertEqual "" (Nothing, Just 5) (d1, d2)

case_asDouble :: IO ()
case_asDouble = do
    let a = parseArray "[3.14,5]"
    let v1 = getArrayElement a 0
    let v2 = getArrayElement a 1
    let d1 = asDouble v1
    let d2 = asDouble v2
    assertEqual "" (Just 3.14, Nothing) (d1, d2)

case_asNumber :: IO ()
case_asNumber = do
    let a = parseArray "[3.14,4]"
    let e1 = asNumber $ getArrayElement a 0
    let e2 = asNumber $ getArrayElement a 1
    assertEqual "" (Just 3.14, Just 4) (e1, e2)

case_getObjectKey :: IO ()
case_getObjectKey = do
    let o = parseObject "{\"foo\":\"bar\"}"
    let k = getObjectKey o 0
    assertEqual "" "foo" k

case_getObjectValue :: IO ()
case_getObjectValue = do
    let Right r = parse "{\"foo\":\"bar\"}"
    let Just o = asObject r
    assertEqual "" 1 (numKeys o)
    let k = getObjectKey o 0
    let v = asString $ getObjectValue o 0
    assertEqual "" "foo" k
    assertEqual "" (Just "bar") v

case_indexOfObjectKey :: IO ()
case_indexOfObjectKey = do
    let o = parseObject "{\"foo\":\"bar\",\"baz\":999,\"quux\":null}"
    let Just index = indexOfObjectKey o "baz"
    let k = getObjectKey o index
    let v = getObjectValue o index
    let vi = asInt v
    assertEqual "" "baz" k
    assertEqual "" (Just 999) vi

case_arrayEquality :: IO ()
case_arrayEquality = do
    let a = parseArray "[1,2,3,4,5]"
    let b = parseArray "[1,2,4,3,5]"
    let c = parseArray "[9,8,7]"
    assertEqual "" a a
    assertEqual "" b b
    assertEqual "" c c
    assertNotEqual a b
    assertNotEqual a c
    assertNotEqual b c
    assertNotEqual b a
    assertNotEqual c a
    assertNotEqual c b

case_objectEquality :: IO ()
case_objectEquality = do
    let a = parseObject "{\"foo\":\"bar\"}"
    let b = parseObject "{\"count\":9000,\"items\":[1,2,3,4]}"
    let c = parseObject "{\"count\":9000,\"items\":[1,2,4,3]}"
    assertEqual "" a a
    assertEqual "" b b
    assertEqual "" c c
    assertNotEqual a b
    assertNotEqual a c
    assertNotEqual b c
    assertNotEqual b a
    assertNotEqual c a
    assertNotEqual c b

case_getObjectWithKey :: IO ()
case_getObjectWithKey = do
    let o = parseObject "{\"foo\":\"bar\",\"baz\":999,\"quux\":null}"
    let v = getObjectWithKey o "quux"
    assertEqual "" (Just TNull) (fmap typeOf v)
    assertEqual "" Nothing (getObjectWithKey o "does not exist")

data Person = Person
    { pName :: Text
    , pAge :: Int
    , pHobbies :: [Text]
    , pMotorcycles :: [Motorcycle]
    } deriving (Show, Eq)

instance FromJson Person where
    fromJson v = withObject v $ \o -> do
        pName <- getKey o "name"
        pAge <- getKey o "age"
        pHobbies <- getKey o "hobbies"
        pMotorcycles <- getKey o "motorcycles"
        return Person {..}

data Motorcycle = Motorcycle
    { mMake :: Text
    , mYear :: Int
    , mLoudness :: Double
    } deriving (Show, Eq)

instance FromJson Motorcycle where
    fromJson v = withObject v $ \o -> do
        mMake <- getKey o "make"
        mYear <- getKey o "year"
        mLoudness <- getKey o "loudness"
        return Motorcycle {..}

case_fromJson = do
    let Right r = parse "{\"name\":\"andy\",\"age\":9000000,\"hobbies\":[\"haskell\",\"kabuki\"],\"motorcycles\":[{\"make\":\"kawasaki\",\"year\":1980,\"loudness\":9000000}]}"
    let o = r
    let p = fromJson o :: Either String Person
    let expected = Right Person
            { pName = "andy"
            , pAge = 9000000
            , pHobbies = ["haskell", "kabuki"]
            , pMotorcycles =
                [ Motorcycle
                    { mMake = "kawasaki"
                    , mYear = 1980
                    , mLoudness = 9000000
                    }
                ]
            }

    assertEqual "" expected p

    return ()

tests :: TestTree
tests = $(testGroupGenerator)

main :: IO ()
main = Tasty.defaultMain tests
