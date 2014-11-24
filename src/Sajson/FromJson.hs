module Sajson.FromJson where

import Sajson hiding (ParseError)
import Data.Text (Text)

type DecodeError = String

class FromJson a where
    fromJson :: Value -> Either DecodeError a

instance FromJson Bool where
    fromJson v = case typeOf v of
        TFalse -> Right False
        TTrue -> Right True
        _ -> Left $ "Expected bool got " ++ (show v)

instance FromJson Int where
    fromJson v = case asInt v of
        Just i -> Right i
        Nothing -> Left $ "Expected int got " ++ (show v)

instance FromJson Text where
    fromJson v = case asString v of
        Just s -> Right s
        Nothing -> Left $ "Expected string got " ++ (show v)

instance FromJson Double where
    fromJson v = case asNumber v of
        Just d -> Right d
        Nothing -> Left $ "Expected double got " ++ (show v)

instance FromJson a => FromJson [a] where
    fromJson v = case asArray v of
        Nothing ->
            Left $ "Expected array got " ++ (show v)
        Just a ->
            sequence [fromJson $ getArrayElement a i | i <- [0..Sajson.length a - 1]]

instance FromJson a => FromJson (Maybe a) where
    fromJson v
        | TNull == typeOf v = Right Nothing
        | otherwise = fmap Just $ fromJson v

withObject :: Value -> (Object -> Either DecodeError a) -> Either DecodeError a
withObject v f = case asObject v of
    Just o -> f o
    Nothing -> Left $ "Expected object, got " ++ (show v)

getKey :: FromJson a => Object -> Text -> Either DecodeError a
getKey a key = case getObjectWithKey a key of
    Just o -> fromJson o
    Nothing -> Left $ "Key " ++ (show key) ++ " not found"

getOptionalKey :: FromJson a => Object -> Text -> Either DecodeError (Maybe a)
getOptionalKey a key = case getObjectWithKey a key of
    Nothing -> Right Nothing
    Just o -> fmap Just $ fromJson o
