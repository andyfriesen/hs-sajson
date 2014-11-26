{-# LANGUAGE OverloadedStrings, RecordWildCards, NamedFieldPuns #-}

module Sajson.ToJson where

import Data.List (foldl')
import Data.Monoid ((<>), mempty)
import Data.Char (ord)
import Data.Text (Text)
import Data.Word (Word8)
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import           Data.ByteString.Builder.Prim as BP
import qualified Data.ByteString.Lazy as BSL

import qualified Data.ByteString.Builder as B

data ObjectBuilder = ObjectBuilder
    { obBuilder :: !B.Builder
    , obNeedsComma :: !Bool
    }

newtype ValueBuilder = ValueBuilder B.Builder

newtype PairBuilder = PairBuilder B.Builder

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

object :: ObjectBuilder -> ValueBuilder
object ObjectBuilder {obBuilder} = ValueBuilder (obBuilder <> closeCurly)

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
{-# INLINE escapeText #-}

ascii2 :: (Char, Char) -> BP.BoundedPrim a
ascii2 cs = BP.liftFixedToBounded $ (const cs) BP.>$< BP.char7 BP.>*< BP.char7
{-# INLINE ascii2 #-}

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

instance ToJson Bool where
    toJson = bool

instance ToJson Int where
    toJson = int

instance ToJson Float where
    toJson = float

instance ToJson Double where
    toJson = double

instance ToJson Text where
    toJson = text

instance ToJson a => ToJson [a] where
    toJson a = newArray $ map toJson a

mkPair :: ToJson a => Text -> a -> PairBuilder
mkPair k v =
    let ValueBuilder vb = toJson v
    in PairBuilder $ quoteText k <> colon <> vb
{-# INLINE mkPair #-}

(.=) :: ToJson a => Text -> a -> PairBuilder
(.=) = mkPair
{-# INLINE (.=) #-}

encodeJson :: ToJson a => a -> BSL.ByteString
encodeJson v =
    let ValueBuilder builder = toJson v
    in B.toLazyByteString builder

encodeJsonStrict :: ToJson a => a -> BS.ByteString
encodeJsonStrict = BSL.toStrict . encodeJson
