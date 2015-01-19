{-# LANGUAGE OverloadedStrings, RecordWildCards, NamedFieldPuns #-}
{-# LANGUAGE FlexibleInstances, ScopedTypeVariables, FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}

module Sajson.ToJson where

import Data.List (foldl')
import Data.Monoid ((<>), mempty, mconcat)
import Data.List (intersperse)
import Data.Char (ord)
import Data.Text (Text)
import Data.Word (Word8)
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import           Data.ByteString.Builder.Prim as BP
import qualified Data.ByteString.Lazy as BSL

import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Builder.Internal as B

import           Data.Vector (Vector)

import           Data.MonoTraversable (MonoFoldable, Element, onull, ofoldl')

data ObjectBuilder = ObjectBuilder
    { obBuilder :: !B.Builder
    , obNeedsComma :: !Bool
    }

newtype ValueBuilder = ValueBuilder B.Builder

newtype PairBuilder = PairBuilder {unPairBuilder :: B.Builder}

newObject :: ObjectBuilder
newObject = ObjectBuilder {obBuilder = openCurly, obNeedsComma = False}
{-# INLINE newObject #-}

add :: ObjectBuilder -> PairBuilder -> ObjectBuilder
add (ObjectBuilder{..}) (PairBuilder pb) =
    let lhs | obNeedsComma = obBuilder <> comma
            | otherwise = obBuilder
    in ObjectBuilder
        { obBuilder = lhs <> pb
        , obNeedsComma = True
        }
{-# INLINE add #-}

object :: [PairBuilder] -> ValueBuilder
object pairs =
    ValueBuilder $ openCurly <> (mconcat . intersperse comma . map unPairBuilder) pairs <> closeCurly

newArray :: [ValueBuilder] -> ValueBuilder
newArray values = case values of
    [] -> ValueBuilder (openBracket <> closeBracket)
    (ValueBuilder first:rest) -> ValueBuilder $
        openBracket <> first <> foldl' addElement mempty rest <> closeBracket
  where
    addElement :: B.Builder -> ValueBuilder -> B.Builder
    addElement b (ValueBuilder vb) = b <> comma <> vb
{-# INLINE newArray #-}

int :: Integral a => a -> ValueBuilder
int i = ValueBuilder $ B.intDec $ fromIntegral i
{-# INLINE int #-}

bool :: Bool -> ValueBuilder
bool b = ValueBuilder $ B.byteString bstr
  where
    bstr = if b then "true" else "false"
{-# INLINE bool #-}

null :: ValueBuilder
null = ValueBuilder $ B.byteString "null"
{-# INLINE null #-}

float :: Float -> ValueBuilder
float f = ValueBuilder $ B.floatDec f
{-# INLINE float #-}

double :: Double -> ValueBuilder
double d = ValueBuilder $ B.doubleDec d
{-# INLINE double #-}

text :: Text -> ValueBuilder
text t = ValueBuilder $ quoteText t
{-# INLINE text #-}

-- escapeText borrowed from Aeson.

escapeText :: Text -> B.Builder
escapeText t =
    B.char8 '"' <> TE.encodeUtf8BuilderEscaped escapeAscii t <> B.char8 '"'
  where
    escapeAscii :: BP.BoundedPrim Word8
    escapeAscii =
        BP.condB (== c2w '\\'  ) (ascii2 ('\\','\\')) $
        BP.condB (== c2w '\"'  ) (ascii2 ('\\','"' )) $
        BP.condB (>= c2w '\x20') (BP.liftFixedToBounded BP.word8) $
        BP.condB (== c2w '\n'  ) (ascii2 ('\\','n' )) $
        BP.condB (== c2w '\r'  ) (ascii2 ('\\','r' )) $
        BP.condB (== c2w '\t'  ) (ascii2 ('\\','t' )) $
        (BP.liftFixedToBounded hexEscape) -- fallback for chars < 0x20

    c2w = fromIntegral . ord

    hexEscape :: BP.FixedPrim Word8
    hexEscape = (\c -> ('\\', ('u', fromIntegral c))) BP.>$<
        BP.char8 BP.>*< BP.char8 BP.>*< BP.word16HexFixed

    ascii2 :: (Char, Char) -> BP.BoundedPrim a
    ascii2 cs = BP.liftFixedToBounded $ (const cs) BP.>$< BP.char7 BP.>*< BP.char7
    {-# INLINE ascii2 #-}

{-# INLINE escapeText #-}

-- escapeText t = B.byteString $ TE.encodeUtf8 t

-- | Unsafely insert a utf8 string into the JSON document.
-- Do not use this for ByteStrings which are not legal utf8.
unsafeUtf8ByteString :: BS.ByteString -> ValueBuilder
-- unsafeUtf8ByteString bs = ValueBuilder $ quoteText $ TE.decodeUtf8 bs
unsafeUtf8ByteString bs = ValueBuilder $ quote <> BP.primMapByteStringBounded escape bs <> quote
  where
    backslashW8 :: Word8
    backslashW8 = fromIntegral $ fromEnum '\\'

    quoteW8 :: Word8
    quoteW8 = fromIntegral $ fromEnum '"'

    escape :: BP.BoundedPrim Word8
    escape =
        BP.condB (== backslashW8) (fixed2 (backslashW8, backslashW8)) $
        BP.condB (== quoteW8)     (fixed2 (backslashW8, quoteW8)) $
        BP.liftFixedToBounded BP.word8

    {-# INLINE fixed2 #-}
    fixed2 x = liftFixedToBounded $ const x BP.>$< BP.word8 BP.>*< BP.word8

-- | Unsafely insert a literal value into the JSON document.
-- Using this makes it possible to formulate strings that are not legal JSON at all, so be careful.
unsafeBuilder :: B.Builder -> ValueBuilder
unsafeBuilder = ValueBuilder
{-# INLINE unsafeBuilder #-}

-- | Unsafely insert a quoted string into the JSON document.
-- This DOES NOT escape anything.  This function can thus be used to produce illegal JSON strings.
-- Only use it when you know that the string you're emitting cannot contain anything that needs escaping.
unsafeQuotedBuilder :: B.Builder -> ValueBuilder
unsafeQuotedBuilder b = ValueBuilder $
    B.char7 '"' <> b <> B.char7 '"'

quoteText :: Text -> B.Builder
quoteText t = quote <> escapeText t <> quote
{-# INLINE quoteText #-}

openCurly :: B.Builder
openCurly = B.char7 '{'
{-# INLINE openCurly #-}

closeCurly :: B.Builder
closeCurly = B.char7 '}'
{-# INLINE closeCurly #-}

comma :: B.Builder
comma = B.char7 ','
{-# INLINE comma #-}

colon :: B.Builder
colon = B.char7 ':'
{-# INLINE colon #-}

quote :: B.Builder
quote = B.char7 '"'
{-# INLINE quote #-}

openBracket :: B.Builder
openBracket = B.char7 '['
{-# INLINE openBracket #-}

closeBracket :: B.Builder
closeBracket = B.char7 ']'
{-# INLINE closeBracket #-}

class ToJson a where
    toJson :: a -> ValueBuilder

instance ToJson ValueBuilder where
    {-# INLINE toJson #-}
    toJson = id

instance ToJson Bool where
    {-# INLINE toJson #-}
    toJson = bool

instance ToJson Int where
    {-# INLINE toJson #-}
    toJson = int

instance ToJson Float where
    {-# INLINE toJson #-}
    toJson = float

instance ToJson Double where
    {-# INLINE toJson #-}
    toJson = double

instance ToJson Text where
    {-# INLINE toJson #-}
    toJson = text

instance ToJson ObjectBuilder where
    {-# INLINE toJson #-}
    toJson ObjectBuilder{..} = ValueBuilder obBuilder

arrayToJson :: (ToJson (Element collection), MonoFoldable collection) => collection -> ValueBuilder
arrayToJson arr
    | onull arr = newArray []
    | otherwise =
        let foldIt (!needComma, !accumulator) !el =
                let ValueBuilder eb = toJson el
                    newBuilder = if needComma
                        then accumulator <> comma <> eb
                        else accumulator <> eb
                in (True, newBuilder)

            joined :: B.Builder
            joined = snd $ ofoldl' foldIt (False, mempty) arr

        in ValueBuilder $ openBracket <> joined <> closeBracket

instance ToJson value => ToJson [value] where
    {-# INLINE toJson #-}
    toJson l = {-# SCC list_to_json #-} arrayToJson l

instance ToJson value => ToJson (Vector value) where
    {-# INLINE toJson #-}
    toJson v = {-# SCC vector_to_json #-} arrayToJson v

mkPair :: ToJson a => Text -> a -> PairBuilder
mkPair k v =
    let ValueBuilder vb = toJson v
    in PairBuilder $ quoteText k <> colon <> vb
{-# INLINE mkPair #-}

mkBSPair :: ToJson a => BS.ByteString -> a -> PairBuilder
mkBSPair k v =
    let ValueBuilder kb = unsafeUtf8ByteString k
        ValueBuilder vb = toJson v
    in PairBuilder $ kb <> colon <> vb
{-# INLINE mkBSPair #-}

(.=) :: ToJson a => Text -> a -> PairBuilder
(.=) = mkPair
{-# INLINE (.=) #-}

(!.=) :: ToJson a => B.Builder -> a -> PairBuilder
key !.= value =
    let ValueBuilder vb = toJson value
        ValueBuilder kb = unsafeQuotedBuilder key
    in PairBuilder $ kb <> colon <> vb
{-# INLINE (!.=) #-}

encodeJsonBuilder :: ToJson a => a -> B.Builder
encodeJsonBuilder v =
    let ValueBuilder builder = toJson v
    in builder

encodeJson :: ToJson a => a -> BSL.ByteString
encodeJson v =
    let ValueBuilder builder = toJson v
        allocationStrategy = B.untrimmedStrategy (100 * kb) (100 * kb)
        kb = 1024

    in B.toLazyByteStringWith allocationStrategy mempty builder

encodeJsonStrict :: ToJson a => a -> BS.ByteString
encodeJsonStrict = BSL.toStrict . encodeJson
