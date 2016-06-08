{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
module Data.Aeson.Encoding.Internal
    (
    -- * Encoding
      Encoding (..) -- TODO: export fromEncoding for now
    , encodingToLazyByteString 
    , unsafeToEncoding
    , Series (..) -- TODO: don't export constructor
    , pairs
    , pair
    -- * Predicates
    , nullEncoding
    -- * Encoding constructors
    , emptyArray_
    , emptyObject_
    , wrapArray
    , wrapObject
    , null_
    , bool
    , text
    , lazyText
    , string
    , list
    , dict
    , tuple
    , (>*<)
    -- ** Decimal numbers
    , int8, int16, int32, int64, int
    , word8, word16, word32, word64, word
    , integer, float, double, scientific
    -- ** Decimal numbers as Text
    , int8Text, int16Text, int32Text, int64Text, intText
    , word8Text, word16Text, word32Text, word64Text, wordText
    , integerText, floatText, doubleText, scientificText
    -- ** Time
    , day
    , localTime
    , utcTime
    , timeOfDay
    , zonedTime
    -- ** value
    , value
    -- ** JSON tokens
    , comma, colon, openBracket, closeBracket, openCurly, closeCurly
    ) where

import Prelude        ()
import Prelude.Compat

import Data.ByteString.Builder      (Builder, char7, toLazyByteString)
import Data.ByteString.Builder.Prim (primBounded)
import Data.Int
import Data.Scientific              (Scientific)
import Data.Semigroup               (Semigroup ((<>)))
import Data.Text                    (Text)
import Data.Time                    (Day, LocalTime, TimeOfDay, UTCTime,
                                     ZonedTime)
import Data.Typeable                (Typeable)
import Data.Word

import Data.Aeson.Types.Internal (Value)

import qualified Data.Aeson.Encoding.Builder as EB
import qualified Data.ByteString.Builder     as B
import qualified Data.ByteString.Lazy        as BSL
import qualified Data.Text.Lazy              as LT

-- | An encoding of a JSON value.
newtype Encoding = Encoding {
      fromEncoding :: Builder
      -- ^ Acquire the underlying bytestring builder.
    } deriving (Semigroup,Monoid,Typeable)

-- | Make Encoding from Builder.
--
-- Use with care! You have to make sure that the passed Builder
-- is a valid JSON Encoding!
unsafeToEncoding :: Builder -> Encoding
unsafeToEncoding = Encoding

encodingToLazyByteString :: Encoding -> BSL.ByteString
encodingToLazyByteString = toLazyByteString . fromEncoding
{-# INLINE encodingToLazyByteString #-}

-------------------------------------------------------------------------------
-- Encoding instances
-------------------------------------------------------------------------------

instance Show Encoding where
    show (Encoding e) = show (toLazyByteString e)

instance Eq Encoding where
    Encoding a == Encoding b = toLazyByteString a == toLazyByteString b

instance Ord Encoding where
    compare (Encoding a) (Encoding b) =
      compare (toLazyByteString a) (toLazyByteString b)

-- | A series of values that, when encoded, should be separated by
-- commas. Since 0.11.0.0, the '.=' operator is overloaded to create
-- either @(Text, Value)@ or 'Series'. You can use Series when
-- encoding directly to a bytestring builder as in the following
-- example:
--
-- > toEncoding (Person name age) = pairs ("name" .= name <> "age" .= age)
data Series = Empty
            | Value Encoding
            deriving (Typeable)

pair :: Text -> Encoding -> Series
pair name val = Value $ text name <> colon <> val

instance Semigroup Series where
    Empty   <> a = a
    Value a <> b =
        Value $
        a <> case b of
               Empty   -> mempty
               Value c -> Encoding (char7 ',') <> c

instance Monoid Series where
    mempty  = Empty
    mappend = (<>)

nullEncoding :: Encoding -> Bool
nullEncoding = BSL.null . toLazyByteString . fromEncoding

emptyArray_ :: Encoding
emptyArray_ = Encoding EB.emptyArray_

emptyObject_ :: Encoding
emptyObject_ = Encoding EB.emptyObject_

wrapArray :: Encoding -> Encoding
wrapArray e = openBracket <> e <> closeBracket

wrapObject :: Encoding -> Encoding
wrapObject e = openCurly <> e <> closeCurly

null_ :: Encoding
null_ = Encoding EB.null_

bool :: Bool -> Encoding
bool True = Encoding "true"
bool False = Encoding "false"

-- | Encode a series of key/value pairs, separated by commas.
pairs :: Series -> Encoding
pairs = brackets '{' '}'
{-# INLINE pairs #-}

brackets :: Char -> Char -> Series -> Encoding
brackets begin end (Value v) = Encoding $
                               char7 begin <> fromEncoding v <> char7 end
brackets begin end Empty     = Encoding (primBounded (EB.ascii2 (begin,end)) ())

list :: (a -> Encoding) -> [a] -> Encoding
list _  []     = emptyArray_
list to' (x:xs) = Encoding $
    char7 '[' <> fromEncoding (to' x) <> commas xs <> char7 ']'
  where
    commas = foldr (\v vs -> char7 ',' <> fromEncoding (to' v) <> vs) mempty
{-# INLINE list #-}

-- | Encode as JSON object
dict
    :: (k -> Encoding)                                -- ^ key encoding
    -> (v -> Encoding)                                -- ^ value encoding
    -> (forall a. (k -> v -> a -> a) -> a -> m -> a)  -- ^ @foldrWithKey@ - indexed fold
    -> m                                              -- ^ container
    -> Encoding
dict encodeKey encodeVal foldrWithKey = brackets '{' '}' . foldrWithKey go mempty
  where
    go k v c = Value (encodeKV k v) <> c
    encodeKV k v = encodeKey k <> Encoding (char7 ':') <> encodeVal v
{-# INLINE dict #-}

infixr 6 >*<
-- | See 'tuple'.
(>*<) :: Encoding -> Encoding -> Encoding
Encoding a >*< Encoding b = Encoding $ a <> char7 ',' <> b
{-# INLINE (>*<) #-}

-- | Encode as a tuple.
--
-- @
-- toEncoding (X a b c) = tuple $
--     toEncoding a >*<
--     toEncoding b >*<
--     toEncoding c
tuple :: Encoding -> Encoding
tuple b = Encoding (char7 '[' <> fromEncoding b <> char7 ']')
{-# INLINE tuple #-}

text :: Text -> Encoding
text = Encoding . EB.text

lazyText :: LT.Text -> Encoding
lazyText t = Encoding $
    B.char7 '"' <>
    LT.foldrChunks (\x xs -> EB.unquoted x <> xs) (B.char7 '"') t

string :: String -> Encoding
string = Encoding . EB.string

-------------------------------------------------------------------------------
-- chars
-------------------------------------------------------------------------------

comma, colon, openBracket, closeBracket, openCurly, closeCurly :: Encoding
comma        = Encoding $ char7 ','
colon        = Encoding $ char7 ':'
openBracket  = Encoding $ char7 '['
closeBracket = Encoding $ char7 ']'
openCurly    = Encoding $ char7 '{'
closeCurly   = Encoding $ char7 '}'

-------------------------------------------------------------------------------
-- Decimal numbers
-------------------------------------------------------------------------------

int8 :: Int8 -> Encoding
int8 = Encoding . B.int8Dec

int16 :: Int16 -> Encoding
int16 = Encoding . B.int16Dec

int32 :: Int32 -> Encoding
int32 = Encoding . B.int32Dec

int64 :: Int64 -> Encoding
int64 = Encoding . B.int64Dec

int :: Int -> Encoding
int = Encoding . B.intDec

word8 :: Word8 -> Encoding
word8 = Encoding . B.word8Dec

word16 :: Word16 -> Encoding
word16 = Encoding . B.word16Dec

word32 :: Word32 -> Encoding
word32 = Encoding . B.word32Dec

word64 :: Word64 -> Encoding
word64 = Encoding . B.word64Dec

word :: Word -> Encoding
word = Encoding . B.wordDec

integer :: Integer -> Encoding
integer = Encoding . B.integerDec

float :: Float -> Encoding
float = realFloatToEncoding $ Encoding . B.floatDec

double :: Double -> Encoding
double = realFloatToEncoding $ Encoding . B.doubleDec

scientific :: Scientific -> Encoding
scientific = Encoding . EB.scientific

realFloatToEncoding :: RealFloat a => (a -> Encoding) -> a -> Encoding
realFloatToEncoding e d
    | isNaN d || isInfinite d = null_
    | otherwise               = e d
{-# INLINE realFloatToEncoding #-}

-------------------------------------------------------------------------------
-- Decimal numbers as Text
-------------------------------------------------------------------------------

int8Text :: Int8 -> Encoding
int8Text = Encoding . EB.quote . B.int8Dec

int16Text :: Int16 -> Encoding
int16Text = Encoding . EB.quote . B.int16Dec

int32Text :: Int32 -> Encoding
int32Text = Encoding . EB.quote . B.int32Dec

int64Text :: Int64 -> Encoding
int64Text = Encoding . EB.quote . B.int64Dec

intText :: Int -> Encoding
intText = Encoding . EB.quote . B.intDec

word8Text :: Word8 -> Encoding
word8Text = Encoding . EB.quote . B.word8Dec

word16Text :: Word16 -> Encoding
word16Text = Encoding . EB.quote . B.word16Dec

word32Text :: Word32 -> Encoding
word32Text = Encoding . EB.quote . B.word32Dec

word64Text :: Word64 -> Encoding
word64Text = Encoding . EB.quote . B.word64Dec

wordText :: Word -> Encoding
wordText = Encoding . EB.quote . B.wordDec

integerText :: Integer -> Encoding
integerText = Encoding . EB.quote . B.integerDec

-- | TODO: check infinite and nan?
floatText :: Float -> Encoding
floatText = Encoding . EB.quote . B.floatDec

doubleText :: Double -> Encoding
doubleText = Encoding . EB.quote . B.doubleDec

scientificText :: Scientific -> Encoding
scientificText = Encoding . EB.quote . EB.scientific

-------------------------------------------------------------------------------
-- time
-------------------------------------------------------------------------------

day :: Day -> Encoding
day = Encoding . EB.quote . EB.day

localTime :: LocalTime -> Encoding
localTime = Encoding . EB.quote . EB.localTime

utcTime :: UTCTime -> Encoding
utcTime = Encoding . EB.quote . EB.utcTime

timeOfDay :: TimeOfDay -> Encoding
timeOfDay = Encoding . EB.quote . EB.timeOfDay

zonedTime :: ZonedTime -> Encoding
zonedTime = Encoding . EB.quote . EB.zonedTime

-------------------------------------------------------------------------------
-- Value
-------------------------------------------------------------------------------

value :: Value -> Encoding
value = Encoding . EB.encodeToBuilder
