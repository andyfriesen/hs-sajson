
module Sajson
    ( Document
    , Value
    , Array
    , Object
    , ParseError (..)
    , Type (..)
    , parse
    , root
    , typeOf
    , asArray
    , asObject
    , asInt
    , asDouble
    , asNumber
    , asString
    , getArrayElement
    , Sajson.length
    , numKeys
    , getObjectKey
    , getObjectValue
    , indexOfObjectKey
    , getObjectWithKey
    ) where

import Prelude hiding (length)
import Control.DeepSeq (force)
import System.IO.Unsafe (unsafePerformIO)
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc (alloca)
import Foreign.C.Types (CChar)
import Foreign.Storable
import Data.Text (Text)
import Data.Text.Foreign
import Data.Word
import Data.Text.Encoding
import Data.ByteString.Unsafe

data CppParser
data CppDocument
data CppValue

-- I am not sure if this is correct.
-- The lifetime of documents and values is dependent on the lifetime of the parser, so
-- all Documents and Values retain the ForeignPtr.
data Document = Document !(ForeignPtr CppParser) !(ForeignPtr CppDocument)
data Value    = Value !(ForeignPtr CppParser) !(ForeignPtr CppValue)

newtype Array = Array Value
    deriving (Eq, Show)
newtype Object = Object Value
    deriving (Eq, Show)

data ParseError = ParseError Int Int Text
    deriving (Show, Eq)

data Type = TInteger | TDouble | TNull | TFalse | TTrue | TString | TArray | TObject
    deriving (Show, Eq)

typeFromInt :: (Show i, Integral i) => i -> Type
typeFromInt p = 
    case p of
        0 -> TInteger
        1 -> TDouble
        2 -> TNull
        3 -> TFalse
        4 -> TTrue
        5 -> TString
        6 -> TArray
        7 -> TObject
        _ -> error $ "Unexpected type from sajson: " ++ (show p)

{-
intFromType :: Integral i => Type -> i
intFromType ty = case ty of
    TInteger -> 0
    TDouble  -> 1
    TNull    -> 2
    TFalse   -> 3
    TTrue    -> 4
    TString  -> 5
    TArray   -> 6
    TObject  -> 7
-}

foreign import ccall unsafe "sj_parser"                     sj_parser                       :: Word -> (Ptr CChar) -> IO (Ptr CppParser)
foreign import ccall unsafe "&sj_parser_free"               sj_parser_free                  :: FunPtr (Ptr CppParser -> IO ())
foreign import ccall unsafe "sj_parser_get_document"        sj_parser_get_document          :: Ptr CppParser -> IO (Ptr CppDocument)
foreign import ccall unsafe "&sj_document_free"             sj_document_free                :: FunPtr (Ptr CppDocument -> IO ())
foreign import ccall unsafe "sj_document_is_valid"          sj_document_is_valid            :: Ptr CppDocument -> IO Bool
foreign import ccall unsafe "sj_document_get_error_line"    sj_document_get_error_line      :: Ptr CppDocument -> IO Int
foreign import ccall unsafe "sj_document_get_error_column"  sj_document_get_error_column    :: Ptr CppDocument -> IO Int
foreign import ccall unsafe "sj_document_get_error_message" sj_document_get_error_message   :: Ptr CppDocument -> IO (Ptr CChar)
foreign import ccall unsafe "sj_document_get_root"          sj_document_get_root            :: Ptr CppDocument -> IO (Ptr CppValue)
foreign import ccall unsafe "&sj_value_free"                sj_value_free                   :: FunPtr (Ptr CppValue -> IO ())
foreign import ccall unsafe "sj_value_get_type"             sj_value_get_type               :: Ptr CppValue -> IO Int
foreign import ccall unsafe "sj_value_get_length"           sj_value_get_length             :: Ptr CppValue -> IO Word
foreign import ccall unsafe "sj_value_get_array_element"    sj_value_get_array_element      :: Ptr CppValue -> Word -> IO (Ptr CppValue)
foreign import ccall unsafe "sj_value_get_integer_value"    sj_value_get_integer_value      :: Ptr CppValue -> IO Int
foreign import ccall unsafe "sj_value_get_number_value"     sj_value_get_number_value       :: Ptr CppValue -> IO Double
foreign import ccall unsafe "sj_value_get_double_value"     sj_value_get_double_value       :: Ptr CppValue -> IO Double
foreign import ccall unsafe "sj_value_get_string_value"     sj_value_get_string_value       :: Ptr CppValue -> Ptr (Ptr CChar) -> Ptr Word -> IO ()
foreign import ccall unsafe "sj_value_get_object_key"       sj_value_get_object_key         :: Ptr CppValue -> Word -> Ptr (Ptr CChar) -> Ptr Word -> IO ()
foreign import ccall unsafe "sj_value_get_object_value"     sj_value_get_object_value       :: Ptr CppValue -> Word -> IO (Ptr CppValue)
foreign import ccall unsafe "sj_value_find_object_key"      sj_value_find_object_key        :: Ptr CppValue -> Ptr CChar -> Word -> IO Word
foreign import ccall unsafe "sj_value_get_object_with_key"  sj_value_get_object_with_key    :: Ptr CppValue -> Ptr CChar -> Word -> IO (Ptr CppValue)

parse :: Text -> Either ParseError Document
parse t = unsafePerformIO $ do
    parserPtr <- withCStringLen t $ \(ptr, len) ->
        sj_parser (fromIntegral len) ptr
    p <- newForeignPtr sj_parser_free $ parserPtr

    withForeignPtr p $ \ptr -> do
        docPtr <- sj_parser_get_document ptr
        isValid <- sj_document_is_valid docPtr

        if isValid
            then do
                d <- newForeignPtr sj_document_free docPtr
                return $ Right $ Document p d
            else do
                lineNo <- sj_document_get_error_line docPtr
                colNo  <- sj_document_get_error_column docPtr
                errPtr <- sj_document_get_error_message docPtr
                errMsg <- unsafePackCString errPtr
                return $ Left $ ParseError lineNo colNo (force $ decodeUtf8 errMsg)

mkValue :: ForeignPtr CppParser -> Ptr CppValue -> IO Value
mkValue pp vp = do
    fp <- newForeignPtr sj_value_free vp
    return $ Value pp fp

root :: Document -> Value
root (Document parserPtr docPtr) = unsafePerformIO $ do
    withForeignPtr docPtr $ \dp -> do
        vp <- sj_document_get_root dp
        mkValue parserPtr vp

withValPtr :: Value -> (Ptr CppValue -> IO a) -> IO a
withValPtr (Value _ valPtr) f =
    withForeignPtr valPtr f

typeOf :: Value -> Type
typeOf (Value _ valPtr) = unsafePerformIO $
    withForeignPtr valPtr $ \vp -> do
        fmap typeFromInt $ sj_value_get_type vp

asArray :: Value -> Maybe Array
asArray val =
    case typeOf val of
        TArray -> Just $ Array val
        _      -> Nothing

asObject :: Value -> Maybe Object
asObject val =
    case typeOf val of
        TObject -> Just $ Object val
        _       -> Nothing

asInt :: Value -> Maybe Int
asInt val =
    case typeOf val of
        TInteger -> unsafePerformIO $
            fmap Just $ withValPtr val sj_value_get_integer_value
        _ ->
            Nothing

asNumber :: Value -> Maybe Double
asNumber val =
    case typeOf val of
        TDouble -> go
        TInteger -> go
        _ -> Nothing
  where
    go = unsafePerformIO $
        fmap Just $ withValPtr val sj_value_get_number_value

asDouble :: Value -> Maybe Double
asDouble val =
    case typeOf val of
        TDouble -> unsafePerformIO $
            fmap Just $ withValPtr val sj_value_get_double_value
        _ ->
            Nothing

unsafeAsString :: Value -> Text
unsafeAsString val = unsafePerformIO $ do
    alloca $ \cpp ->
        alloca $ \lp -> do
            withValPtr val $ \vp -> sj_value_get_string_value vp cpp lp
            cp <- peek cpp
            l <- peek lp
            bs <- unsafePackCStringLen (cp, fromIntegral l)
            return $ force $ decodeUtf8 bs

asString :: Value -> Maybe Text
asString val =
    case typeOf val of
        TString -> Just $ unsafeAsString val
        _       -> Nothing

getArrayElement :: Array -> Word -> Value
getArrayElement (Array (Value parserPtr valPtr)) index = unsafePerformIO $
    withForeignPtr valPtr $ \vp -> do
        result <- sj_value_get_array_element vp index
        mkValue parserPtr result

unsafeLength :: Value -> Word
unsafeLength val = unsafePerformIO $
    withValPtr val $ \vp ->
        sj_value_get_length vp

length :: Array -> Word
length (Array v) = unsafeLength v

numKeys :: Object -> Word
numKeys (Object o) = unsafeLength o

mkText :: Ptr (Ptr CChar) -> Ptr Word -> IO Text
mkText ccp lp = do
    cp <- peek ccp
    l <- peek lp
    bs <- unsafePackCStringLen (cp, fromIntegral l)
    return $ force $ decodeUtf8 bs

getObjectKey :: Object -> Word -> Text
getObjectKey (Object o) index = unsafePerformIO $
    withValPtr o $ \op ->
        alloca $ \ccp ->
            alloca $ \lp -> do
                sj_value_get_object_key op index ccp lp
                mkText ccp lp

getObjectValue :: Object -> Word -> Value
getObjectValue (Object (Value parserPtr valPtr)) index = unsafePerformIO $
    withForeignPtr valPtr $ \vp -> do
        result <- sj_value_get_object_value vp index
        mkValue parserPtr result

indexOfObjectKey :: Object -> Text -> Maybe Word
indexOfObjectKey (Object (Value _ valPtr)) key = unsafePerformIO $
    withForeignPtr valPtr $ \vp -> do
        withCStringLen key $ \(cp, l) -> do
            result <- sj_value_find_object_key vp cp $ fromIntegral l
            nk <- sj_value_get_length vp
            return $ if result < nk then Just result else Nothing

getObjectWithKey :: Object -> Text -> Maybe Value
getObjectWithKey (Object (Value parserPtr valPtr)) key = unsafePerformIO $
    withForeignPtr valPtr $ \vp -> do
        withCStringLen key $ \(cp, l) -> do
            result <- sj_value_get_object_with_key vp cp (fromIntegral l)
            if nullPtr /= result
                then fmap Just $ mkValue parserPtr result
                else return Nothing

instance Show Value where
    show v = case typeOf v of
        TNull -> "null"
        TTrue -> "true"
        TFalse -> "false"
        TInteger -> show i where Just i = asInt v
        TDouble -> show d  where Just d = asDouble v
        TString -> show t  where t = unsafeAsString v
        TArray -> "{Array length=" ++ (show $ unsafeLength v) ++ "}"
        TObject -> "{Object length=" ++ (show $ unsafeLength v) ++ "}"

instance Eq Value where
    -- This could be more efficient by delegating the work to C.
    a == b
        | typeOf a /= typeOf b = False
        | otherwise =
            case typeOf a of
                TNull    -> True
                TTrue    -> True
                TFalse   -> True
                TInteger -> asInt a == asInt b
                TDouble  -> asDouble a == asDouble b
                TString  -> unsafeAsString a == unsafeAsString b
                TArray   -> eqArrays (Array a) (Array b)
                TObject  -> eqObjects (Object a) (Object b)

eqArrays :: Array -> Array -> Bool
eqArrays a b
    | length a /= length b = False
    | otherwise =
        let cmpElement i = (getArrayElement a i) == (getArrayElement b i)
        in and (map cmpElement [0..length a - 1])

eqObjects :: Object -> Object -> Bool
eqObjects a b
    | numKeys a /= numKeys b = False
    | otherwise =
        let cmpElement i =
                ((getObjectKey a i) == (getObjectKey b i)) &&
                ((getObjectValue a i) == (getObjectValue b i))
        in and (map cmpElement [0..numKeys a - 1])
