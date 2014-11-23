
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
    , asString
    , getArrayElement
    , Sajson.length
    , numKeys
    , getObjectKey
    , getObjectValue
    ) where

import Prelude hiding (True, False, length)
import System.IO.Unsafe (unsafePerformIO)
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc (alloca)
import Foreign.C.Types (CChar)
import Foreign.Storable
import Data.Text (Text, pack)
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
newtype Object = Object Value

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

newtype Str = Str (Ptr CChar, Word)

ptrSize :: Int
ptrSize = sizeOf (undefined :: Ptr CChar)

wordSize :: Int
wordSize = sizeOf (undefined :: Word)

instance Storable Str where
    sizeOf _ = ptrSize + wordSize
    alignment (Str (a, _)) = alignment a
    peek ptr = do
        print "peek"
        p <- peek $ castPtr ptr
        print ("p", p)
        l <- peek $ castPtr $ ptr `plusPtr` 1
        print ("l", l)
        return $ Str (p, l)

    poke ptr (Str (p, l)) = do
        print p
        poke (castPtr ptr) p
        print l
        poke (castPtr $ ptr `plusPtr` 1) l

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
foreign import ccall unsafe "sj_value_get_string_value"     sj_value_get_string_value       :: Ptr CppValue -> Ptr Str -> IO ()
foreign import ccall unsafe "sj_value_get_object_key"       sj_value_get_object_key         :: Ptr CppValue -> Word -> Ptr Str -> IO ()
foreign import ccall unsafe "sj_value_get_object_value"     sj_value_get_object_value       :: Ptr CppValue -> Word -> IO (Ptr CppValue)

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
                return $ Left $ ParseError lineNo colNo (decodeUtf8 errMsg)

mkValue pp vp = do
    fp <- newForeignPtr sj_value_free vp
    return $ Value pp fp

root :: Document -> Value
root (Document parserPtr docPtr) = unsafePerformIO $ do
    withForeignPtr docPtr $ \dp -> do
        vp <- sj_document_get_root dp
        mkValue parserPtr vp

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

asString :: Value -> Maybe Text
asString val =
    case typeOf val of
        TString -> unsafePerformIO $ do
            alloca $ \sp -> do
                withValPtr val $ \vp -> sj_value_get_string_value vp sp
                fmap Just $ mkText sp
        _ -> Nothing

getArrayElement :: Array -> Word -> Value
getArrayElement (Array (Value parserPtr valPtr)) index = unsafePerformIO $
    withForeignPtr valPtr $ \vp -> do
        result <- sj_value_get_array_element vp index
        mkValue parserPtr result

unsafeLength val = unsafePerformIO $
    withValPtr val $ \vp ->
        sj_value_get_length vp

length :: Array -> Word
length (Array v) = unsafeLength v

numKeys :: Object -> Word
numKeys (Object o) = unsafeLength o

mkText :: Ptr Str -> IO Text
mkText sp = do
    -- Str (p, l) <- peek sp
    -- bs <- unsafePackCStringLen (p, fromIntegral l)
    return $ pack "ho"
    -- return $ decodeUtf8 bs

getObjectKey :: Object -> Word -> Text
getObjectKey (Object o) index = unsafePerformIO $
    withValPtr o $ \op ->
        alloca $ \sp -> do
            let _ = sp :: Ptr Str
            -- sj_value_get_object_key op index sp
            -- mkText sp
            return $ pack "hoya"

getObjectValue :: Object -> Word -> Value
getObjectValue (Object (Value parserPtr valPtr)) index = unsafePerformIO $
    withForeignPtr valPtr $ \vp -> do
        result <- sj_value_get_object_value vp index
        mkValue parserPtr result
