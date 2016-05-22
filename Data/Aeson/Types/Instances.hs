{-# LANGUAGE CPP, BangPatterns, DeriveDataTypeable, FlexibleContexts,
    FlexibleInstances, GeneralizedNewtypeDeriving,
    OverloadedStrings, UndecidableInstances,
    ViewPatterns #-}
{-# LANGUAGE DefaultSignatures #-}

-- Needed for Tagged, Const and Proxy instances
#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE PolyKinds #-}
#endif

#define NEEDS_INCOHERENT
#include "overlapping-compat.h"

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- TODO: Drop this when we remove support for Data.Attoparsec.Number
{-# OPTIONS_GHC -fno-warn-deprecations #-}

-- |
-- Module:      Data.Aeson.Types.Instances
-- Copyright:   (c) 2011-2016 Bryan O'Sullivan
--              (c) 2011 MailRank, Inc.
-- License:     BSD3
-- Maintainer:  Bryan O'Sullivan <bos@serpentine.com>
-- Stability:   experimental
-- Portability: portable
--
-- Types for working with JSON data.

module Data.Aeson.Types.Instances
    (
    -- * Type classes
    -- ** Core JSON classes
      FromJSON(..)
    , ToJSON(..)
    , KeyValue(..)
    -- ** Liftings to unary and binary type constructors
    , FromJSON1(..)
    , parseJSON1
    , FromJSON2(..)
    , parseJSON2
    , ToJSON1(..)
    , toJSON1
    , toEncoding1
    , ToJSON2(..)
    , toJSON2
    -- ** Generic JSON classes
    , GFromJSON(..)
    , GToJSON(..)
    , GToEncoding(..)
    , genericToJSON
    , genericToEncoding
    , genericParseJSON

    -- * Types
    , DotNetTime(..)

      -- * Inspecting @'Value's@
    , withObject
    , withText
    , withArray
    , withNumber
    , withScientific
    , withBool

    -- * Functions
    , fromJSON
    , ifromJSON
    , (.:)
    , (.:?)
    , (.:!)
    , (.!=)
    , tuple
    , (>*<)
    , typeMismatch
    ) where

import Data.Aeson.Types.Instances.Tuple (tuple, (>*<))

import Control.Applicative (Const(..))
import Data.Aeson.Encode.Functions (brackets, builder, encode, foldable, list)
import Data.Aeson.Functions (hashMapKey, mapHashKeyVal, mapKey, mapKeyVal)
import Data.Aeson.Types.Class
import Data.Aeson.Types.Internal
import Data.Attoparsec.Number (Number(..))
import Data.Fixed (Fixed, HasResolution)
import Data.Foldable (toList)
import Data.Functor.Identity (Identity(..))
import Data.Hashable (Hashable(..))
import Data.Int (Int8, Int16, Int32, Int64)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Monoid (Dual(..), First(..), Last(..))
import Data.Proxy (Proxy(..))
import Data.Ratio (Ratio, (%), numerator, denominator)
import Data.Scientific (Scientific)
import Data.Tagged (Tagged(..))
import Data.Text (Text, pack, unpack)
import Data.Time (Day, LocalTime, NominalDiffTime, TimeOfDay, UTCTime,
                  ZonedTime)
import Data.Time.Format (FormatTime, formatTime, parseTime)
import Data.Traversable as Tr (sequence)
import Data.Vector (Vector)
import Data.Word (Word8, Word16, Word32, Word64)
import Data.Version (Version, showVersion, parseVersion)
import Text.ParserCombinators.ReadP (readP_to_S)
import Foreign.Storable (Storable)
import Numeric.Natural (Natural)
import Prelude hiding (foldr)
import qualified Data.Aeson.Encode.Builder as E
import qualified Data.Aeson.Parser.Time as Time
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as L
import qualified Data.HashMap.Strict as H
import qualified Data.HashSet as HashSet
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map as M
import qualified Data.Scientific as Scientific
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT
import qualified Data.Tree as Tree
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative ((<$>), (<*>), pure)
import Data.Monoid (mempty)
import Data.Word (Word)
#endif

#if MIN_VERSION_time(1,5,0)
import Data.Time.Format (defaultTimeLocale)
#else
import System.Locale (defaultTimeLocale)
#endif

parseIndexedJSON :: FromJSON a => Int -> Value -> Parser a
parseIndexedJSON = parseIndexedJSON' parseJSON

parseIndexedJSON' :: (Value -> Parser a) -> Int -> Value -> Parser a
parseIndexedJSON' p idx value = p value <?> Index idx

instance ToJSON1 Identity where
    liftToJSON to (Identity a) = to a
    {-# INLINE liftToJSON #-}

    liftToEncoding to (Identity a) = to a
    {-# INLINE liftToEncoding #-}

instance (ToJSON a) => ToJSON (Identity a) where
    toJSON = toJSON1 
    {-# INLINE toJSON #-}

    toEncoding = toEncoding1
    {-# INLINE toEncoding #-}

instance FromJSON1 Identity where
    liftParseJSON p a = Identity <$> p a
    {-# INLINE liftParseJSON #-}

instance (FromJSON a) => FromJSON (Identity a) where
    parseJSON = parseJSON1 
    {-# INLINE parseJSON #-}


instance ToJSON1 Maybe where
    liftToJSON to (Just a) = to a
    liftToJSON _  Nothing  = Null
    {-# INLINE liftToJSON #-}

    liftToEncoding to (Just a) = to a
    liftToEncoding _  Nothing  = Encoding E.null_
    {-# INLINE liftToEncoding #-}

instance (ToJSON a) => ToJSON (Maybe a) where
    toJSON = toJSON1
    {-# INLINE toJSON #-}

    toEncoding = toEncoding1
    {-# INLINE toEncoding #-}

instance FromJSON1 Maybe where
    liftParseJSON _ Null = pure Nothing
    liftParseJSON p a    = Just <$> p a
    {-# INLINE liftParseJSON #-}

instance (FromJSON a) => FromJSON (Maybe a) where
    parseJSON = parseJSON1
    {-# INLINE parseJSON #-}


instance ToJSON2 Either where
    liftToJSON2  toA _toB (Left a)  = Object $ H.singleton left  (toA a)
    liftToJSON2 _toA  toB (Right b) = Object $ H.singleton right (toB b)
    {-# INLINE liftToJSON2 #-}

    liftToEncoding2  toA _toB (Left a) =
        Encoding (B.shortByteString "{\"Left\":")
        <> toA a
        <> Encoding (B.char7 '}')

    liftToEncoding2 _toA  toB (Right b) =
        Encoding (B.shortByteString "{\"Right\":")
        <> toB b
        <> Encoding (B.char7 '}')
    {-# INLINE liftToEncoding2 #-}

instance (ToJSON a) => ToJSON1 (Either a) where
    liftToJSON = liftToJSON2 toJSON
    {-# INLINE liftToJSON #-}

    liftToEncoding = liftToEncoding2 toEncoding
    {-# INLINE liftToEncoding #-}

instance (ToJSON a, ToJSON b) => ToJSON (Either a b) where
    toJSON = toJSON2
    {-# INLINE toJSON #-}

    toEncoding = toEncoding2
    {-# INLINE toEncoding #-}

instance FromJSON2 Either where
    liftParseJSON2 pA pB (Object (H.toList -> [(key, value)]))
        | key == left  = Left  <$> pA value <?> Key left
        | key == right = Right <$> pB value <?> Key right

    liftParseJSON2 _ _ _ = fail $
        "expected an object with a single property " ++
        "where the property key should be either " ++
        "\"Left\" or \"Right\""
    {-# INLINE liftParseJSON2 #-}

instance (FromJSON a) => FromJSON1 (Either a) where
    liftParseJSON = liftParseJSON2 parseJSON
    {-# INLINE liftParseJSON #-}

instance (FromJSON a, FromJSON b) => FromJSON (Either a b) where
    parseJSON = parseJSON2
    {-# INLINE parseJSON #-}

left, right :: Text
left  = "Left"
right = "Right"

instance ToJSON Bool where
    toJSON = Bool
    {-# INLINE toJSON #-}

    toEncoding = Encoding . E.bool
    {-# INLINE toEncoding #-}

instance FromJSON Bool where
    parseJSON = withBool "Bool" pure
    {-# INLINE parseJSON #-}

instance ToJSON Ordering where
  toJSON     = toJSON     . orderingToText
  toEncoding = toEncoding . orderingToText

orderingToText :: Ordering -> T.Text
orderingToText o = case o of
                     LT -> "LT"
                     EQ -> "EQ"
                     GT -> "GT"

instance FromJSON Ordering where
  parseJSON = withText "Ordering" $ \s ->
    case s of
      "LT" -> return LT
      "EQ" -> return EQ
      "GT" -> return GT
      _ -> fail "Parsing Ordering value failed: expected \"LT\", \"EQ\", or \"GT\""

instance ToJSON () where
    toJSON _ = emptyArray
    {-# INLINE toJSON #-}

    toEncoding _ = E.emptyArray_
    {-# INLINE toEncoding #-}

instance FromJSON () where
    parseJSON = withArray "()" $ \v ->
                  if V.null v
                    then pure ()
                    else fail "Expected an empty array"
    {-# INLINE parseJSON #-}

instance INCOHERENT_ ToJSON [Char] where
    toJSON = String . T.pack
    {-# INLINE toJSON #-}

    toEncoding = Encoding . E.string
    {-# INLINE toEncoding #-}

instance INCOHERENT_ FromJSON [Char] where
    parseJSON = withText "String" $ pure . T.unpack
    {-# INLINE parseJSON #-}

instance ToJSON Char where
    toJSON = String . T.singleton
    {-# INLINE toJSON #-}

    toEncoding = Encoding . E.string . (:[])
    {-# INLINE toEncoding #-}

instance FromJSON Char where
    parseJSON = withText "Char" $ \t ->
                  if T.compareLength t 1 == EQ
                    then pure $ T.head t
                    else fail "Expected a string of length 1"
    {-# INLINE parseJSON #-}

instance ToJSON Scientific where
    toJSON = Number
    {-# INLINE toJSON #-}

    toEncoding = Encoding . E.number
    {-# INLINE toEncoding #-}

instance FromJSON Scientific where
    parseJSON = withScientific "Scientific" pure
    {-# INLINE parseJSON #-}

instance ToJSON Double where
    toJSON = realFloatToJSON
    {-# INLINE toJSON #-}

    toEncoding = realFloatToEncoding
    {-# INLINE toEncoding #-}

instance FromJSON Double where
    parseJSON = parseRealFloat "Double"
    {-# INLINE parseJSON #-}

instance ToJSON Number where
    toJSON (D d) = toJSON d
    toJSON (I i) = toJSON i
    {-# INLINE toJSON #-}

    toEncoding (D d) = toEncoding d
    toEncoding (I i) = toEncoding i
    {-# INLINE toEncoding #-}

instance FromJSON Number where
    parseJSON (Number s) = pure $ scientificToNumber s
    parseJSON Null       = pure (D (0/0))
    parseJSON v          = typeMismatch "Number" v
    {-# INLINE parseJSON #-}

instance ToJSON Float where
    toJSON = realFloatToJSON
    {-# INLINE toJSON #-}

    toEncoding = realFloatToEncoding
    {-# INLINE toEncoding #-}

instance FromJSON Float where
    parseJSON = parseRealFloat "Float"
    {-# INLINE parseJSON #-}

instance ToJSON (Ratio Integer) where
    toJSON r = object [ "numerator"   .= numerator   r
                      , "denominator" .= denominator r
                      ]
    {-# INLINE toJSON #-}

    toEncoding r = Encoding $
      B.shortByteString "{\"numerator\":" <> builder (numerator r) <>
      B.shortByteString ",\"denominator\":" <> builder (denominator r) <>
      B.char7 '}'
    {-# INLINE toEncoding #-}

instance FromJSON (Ratio Integer) where
    parseJSON = withObject "Rational" $ \obj ->
                  (%) <$> obj .: "numerator"
                      <*> obj .: "denominator"
    {-# INLINE parseJSON #-}

instance HasResolution a => ToJSON (Fixed a) where
    toJSON = Number . realToFrac
    {-# INLINE toJSON #-}

    toEncoding = Encoding . E.number . realToFrac
    {-# INLINE toEncoding #-}

-- | /WARNING:/ Only parse fixed-precision numbers from trusted input
-- since an attacker could easily fill up the memory of the target
-- system by specifying a scientific number with a big exponent like
-- @1e1000000000@.
instance HasResolution a => FromJSON (Fixed a) where
    parseJSON = withScientific "Fixed" $ pure . realToFrac
    {-# INLINE parseJSON #-}

instance ToJSON Int where
    toJSON = Number . fromIntegral
    {-# INLINE toJSON #-}

    toEncoding = Encoding . B.intDec
    {-# INLINE toEncoding #-}

instance FromJSON Int where
    parseJSON = parseIntegral "Int"
    {-# INLINE parseJSON #-}

instance ToJSON Integer where
    toJSON = Number . fromInteger
    {-# INLINE toJSON #-}

    toEncoding = Encoding . B.integerDec
    {-# INLINE toEncoding #-}

-- | /WARNING:/ Only parse Integers from trusted input since an
-- attacker could easily fill up the memory of the target system by
-- specifying a scientific number with a big exponent like
-- @1e1000000000@.
instance FromJSON Integer where
    parseJSON = withScientific "Integral" $ pure . truncate
    {-# INLINE parseJSON #-}

instance ToJSON Natural where
    toJSON = toJSON . toInteger
    {-# INLINE toJSON #-}

    toEncoding = toEncoding . toInteger
    {-# INLINE toEncoding #-}

instance FromJSON Natural where
    parseJSON = withScientific "Natural" $ \s ->
      if Scientific.coefficient s < 0
        then fail $ "Expected a Natural number but got the negative number: " <> show s
        else pure $ truncate s

instance ToJSON Int8 where
    toJSON = Number . fromIntegral
    {-# INLINE toJSON #-}

    toEncoding = Encoding . B.int8Dec
    {-# INLINE toEncoding #-}

instance FromJSON Int8 where
    parseJSON = parseIntegral "Int8"
    {-# INLINE parseJSON #-}

instance ToJSON Int16 where
    toJSON = Number . fromIntegral
    {-# INLINE toJSON #-}

    toEncoding = Encoding . B.int16Dec
    {-# INLINE toEncoding #-}

instance FromJSON Int16 where
    parseJSON = parseIntegral "Int16"
    {-# INLINE parseJSON #-}

instance ToJSON Int32 where
    toJSON = Number . fromIntegral
    {-# INLINE toJSON #-}

    toEncoding = Encoding . B.int32Dec
    {-# INLINE toEncoding #-}

instance FromJSON Int32 where
    parseJSON = parseIntegral "Int32"
    {-# INLINE parseJSON #-}

instance ToJSON Int64 where
    toJSON = Number . fromIntegral
    {-# INLINE toJSON #-}

    toEncoding = Encoding . B.int64Dec
    {-# INLINE toEncoding #-}

instance FromJSON Int64 where
    parseJSON = parseIntegral "Int64"
    {-# INLINE parseJSON #-}

instance ToJSON Word where
    toJSON = Number . fromIntegral
    {-# INLINE toJSON #-}

    toEncoding = Encoding . B.wordDec
    {-# INLINE toEncoding #-}

instance FromJSON Word where
    parseJSON = parseIntegral "Word"
    {-# INLINE parseJSON #-}

instance ToJSON Word8 where
    toJSON = Number . fromIntegral
    {-# INLINE toJSON #-}

    toEncoding = Encoding . B.word8Dec
    {-# INLINE toEncoding #-}

instance FromJSON Word8 where
    parseJSON = parseIntegral "Word8"
    {-# INLINE parseJSON #-}

instance ToJSON Word16 where
    toJSON = Number . fromIntegral
    {-# INLINE toJSON #-}

    toEncoding = Encoding . B.word16Dec
    {-# INLINE toEncoding #-}

instance FromJSON Word16 where
    parseJSON = parseIntegral "Word16"
    {-# INLINE parseJSON #-}

instance ToJSON Word32 where
    toJSON = Number . fromIntegral
    {-# INLINE toJSON #-}

    toEncoding = Encoding . B.word32Dec
    {-# INLINE toEncoding #-}

instance FromJSON Word32 where
    parseJSON = parseIntegral "Word32"
    {-# INLINE parseJSON #-}

instance ToJSON Word64 where
    toJSON = Number . fromIntegral
    {-# INLINE toJSON #-}

    toEncoding = Encoding . B.word64Dec
    {-# INLINE toEncoding #-}

instance FromJSON Word64 where
    parseJSON = parseIntegral "Word64"
    {-# INLINE parseJSON #-}

instance ToJSON Text where
    toJSON = String
    {-# INLINE toJSON #-}

    toEncoding = Encoding . E.text
    {-# INLINE toEncoding #-}

instance FromJSON Text where
    parseJSON = withText "Text" pure
    {-# INLINE parseJSON #-}

instance ToJSON LT.Text where
    toJSON = String . LT.toStrict
    {-# INLINE toJSON #-}

    toEncoding t = Encoding $
      B.char7 '"' <>
      LT.foldrChunks (\x xs -> E.unquoted x <> xs) (B.char7 '"') t
    {-# INLINE toEncoding #-}

instance FromJSON LT.Text where
    parseJSON = withText "Lazy Text" $ pure . LT.fromStrict
    {-# INLINE parseJSON #-}

instance ToJSON1 NonEmpty where
    liftToJSON to = liftToJSON to . toList
    {-# INLINE liftToJSON #-}

    liftToEncoding = foldable
    {-# INLINE liftToEncoding #-}

instance (ToJSON a) => ToJSON (NonEmpty a) where
    toJSON = toJSON1
    {-# INLINE toJSON #-}

    toEncoding = toEncoding1
    {-# INLINE toEncoding #-}

instance FromJSON1 NonEmpty where
    liftParseJSON p = withArray "NonEmpty a" $
        (>>= ne) . Tr.sequence . zipWith (parseIndexedJSON' p) [0..] . V.toList
      where
        ne []     = fail "Expected a NonEmpty but got an empty list"
        ne (x:xs) = pure (x :| xs)
    {-# INLINE liftParseJSON #-}

instance (FromJSON a) => FromJSON (NonEmpty a) where
    parseJSON = parseJSON1

instance ToJSON1 [] where
    liftToJSON to = Array . V.fromList . map to
    {-# INLINE liftToJSON #-}

    liftToEncoding = list
    {-# INLINE liftToEncoding #-}

instance OVERLAPPABLE_ (ToJSON a) => ToJSON [a] where
    toJSON = toJSON1
    {-# INLINE toJSON #-}

    toEncoding = toEncoding1
    {-# INLINE toEncoding #-}

instance FromJSON1 [] where
    liftParseJSON p = withArray "[a]" $
        Tr.sequence . zipWith (parseIndexedJSON' p) [0..] . V.toList
    {-# INLINE liftParseJSON #-}

instance OVERLAPPABLE_ (FromJSON a) => FromJSON [a] where
    parseJSON = parseJSON1
    {-# INLINE parseJSON #-}

instance ToJSON1 Seq.Seq where
    liftToJSON to = liftToJSON to . toList
    {-# INLINE liftToJSON #-}

    liftToEncoding = foldable
    {-# INLINE liftToEncoding #-}

instance (ToJSON a) => ToJSON (Seq.Seq a) where
    toJSON = toJSON1
    {-# INLINE toJSON #-}

    toEncoding = toEncoding1
    {-# INLINE toEncoding #-}

instance FromJSON1 Seq.Seq where
    liftParseJSON p = withArray "Seq a" $
      fmap Seq.fromList .
      Tr.sequence . zipWith (parseIndexedJSON' p) [0..] . V.toList
    {-# INLINE liftParseJSON #-}

instance (FromJSON a) => FromJSON (Seq.Seq a) where
    parseJSON = parseJSON1
    {-# INLINE parseJSON #-}

instance ToJSON1 Vector where
    liftToJSON to = Array . V.map to
    {-# INLINE liftToJSON #-}

    liftToEncoding to xs
        | V.null xs = E.emptyArray_
        | otherwise = Encoding $
            B.char7 '[' <> fromEncoding (to (V.unsafeHead xs)) <>
            V.foldr go (B.char7 ']') (V.unsafeTail xs)
          where
            go v b = B.char7 ',' <> fromEncoding (to v) <> b
    {-# INLINE liftToEncoding #-}

instance (ToJSON a) => ToJSON (Vector a) where
    toJSON = Array . V.map toJSON
    {-# INLINE toJSON #-}

    toEncoding = encodeVector
    {-# INLINE toEncoding #-}

encodeVector :: (ToJSON a, VG.Vector v a) => v a -> Encoding
encodeVector xs
  | VG.null xs = E.emptyArray_
  | otherwise  = Encoding $
                 B.char7 '[' <> builder (VG.unsafeHead xs) <>
                 VG.foldr go (B.char7 ']') (VG.unsafeTail xs)
    where go v b = B.char7 ',' <> builder v <> b
{-# INLINE encodeVector #-}

instance FromJSON1 Vector where
    liftParseJSON p =  withArray "Vector a" $
        V.mapM (uncurry $ parseIndexedJSON' p) . V.indexed
    {-# INLINE liftParseJSON #-}

instance (FromJSON a) => FromJSON (Vector a) where
    parseJSON = parseJSON1
    {-# INLINE parseJSON #-}

vectorToJSON :: (VG.Vector v a, ToJSON a) => v a -> Value
vectorToJSON = Array . V.map toJSON . V.convert
{-# INLINE vectorToJSON #-}

vectorParseJSON :: (FromJSON a, VG.Vector w a) => String -> Value -> Parser (w a)
vectorParseJSON s = withArray s $ fmap V.convert . V.mapM (uncurry parseIndexedJSON) . V.indexed
{-# INLINE vectorParseJSON #-}

instance (Storable a, ToJSON a) => ToJSON (VS.Vector a) where
    toJSON = vectorToJSON
    {-# INLINE toJSON #-}

    toEncoding = encodeVector
    {-# INLINE toEncoding #-}

instance (Storable a, FromJSON a) => FromJSON (VS.Vector a) where
    parseJSON = vectorParseJSON "Data.Vector.Storable.Vector a"

instance (VP.Prim a, ToJSON a) => ToJSON (VP.Vector a) where
    toJSON = vectorToJSON
    {-# INLINE toJSON #-}

    toEncoding = encodeVector
    {-# INLINE toEncoding #-}

instance (VP.Prim a, FromJSON a) => FromJSON (VP.Vector a) where
    parseJSON = vectorParseJSON "Data.Vector.Primitive.Vector a"
    {-# INLINE parseJSON #-}

instance (VG.Vector VU.Vector a, ToJSON a) => ToJSON (VU.Vector a) where
    toJSON = vectorToJSON
    {-# INLINE toJSON #-}

    toEncoding = encodeVector
    {-# INLINE toEncoding #-}

instance (VG.Vector VU.Vector a, FromJSON a) => FromJSON (VU.Vector a) where
    parseJSON = vectorParseJSON "Data.Vector.Unboxed.Vector a"
    {-# INLINE parseJSON #-}

instance (ToJSON a) => ToJSON (Set.Set a) where
    toJSON = toJSON . Set.toList
    {-# INLINE toJSON #-}

    toEncoding = encodeSet Set.minView Set.foldr
    {-# INLINE toEncoding #-}

instance (Ord a, FromJSON a) => FromJSON (Set.Set a) where
    parseJSON = fmap Set.fromList . parseJSON
    {-# INLINE parseJSON #-}

instance (ToJSON a) => ToJSON (HashSet.HashSet a) where
    toJSON = toJSON . HashSet.toList
    {-# INLINE toJSON #-}

    toEncoding = foldable toEncoding
    {-# INLINE toEncoding #-}

instance (Eq a, Hashable a, FromJSON a) => FromJSON (HashSet.HashSet a) where
    parseJSON = fmap HashSet.fromList . parseJSON
    {-# INLINE parseJSON #-}

instance ToJSON IntSet.IntSet where
    toJSON = toJSON . IntSet.toList
    {-# INLINE toJSON #-}

    toEncoding = encodeSet IntSet.minView IntSet.foldr
    {-# INLINE toEncoding #-}

encodeSet :: (ToJSON a) =>
             (s -> Maybe (a, s))
          -> ((a -> B.Builder -> B.Builder) -> B.Builder -> s -> B.Builder)
          -> s -> Encoding
encodeSet minView foldr xs =
    case minView xs of
      Nothing     -> E.emptyArray_
      Just (m,ys) -> Encoding $
                     B.char7 '[' <> builder m <> foldr go (B.char7 ']') ys
        where go v b = B.char7 ',' <> builder v <> b
{-# INLINE encodeSet #-}

instance FromJSON IntSet.IntSet where
    parseJSON = fmap IntSet.fromList . parseJSON
    {-# INLINE parseJSON #-}

instance ToJSON a => ToJSON (IntMap.IntMap a) where
    toJSON = toJSON . IntMap.toList
    {-# INLINE toJSON #-}

    toEncoding = toEncoding . IntMap.toList
    {-# INLINE toEncoding #-}

instance FromJSON a => FromJSON (IntMap.IntMap a) where
    parseJSON = fmap IntMap.fromList . parseJSON
    {-# INLINE parseJSON #-}

instance (ToJSON v) => ToJSON (M.Map Text v) where
    toJSON = Object . M.foldrWithKey (\k -> H.insert k . toJSON) H.empty
    {-# INLINE toJSON #-}

    toEncoding = encodeMap M.minViewWithKey M.foldrWithKey
    {-# INLINE toEncoding #-}

encodeMap :: (ToJSON k, ToJSON v) =>
             (m -> Maybe ((k,v), m))
          -> ((k -> v -> B.Builder -> B.Builder) -> B.Builder -> m -> B.Builder)
          -> m -> Encoding
encodeMap minViewWithKey foldrWithKey xs =
    case minViewWithKey xs of
      Nothing         -> E.emptyObject_
      Just ((k,v),ys) -> Encoding $
                         B.char7 '{' <> encodeKV k v <>
                         foldrWithKey go (B.char7 '}') ys
  where go k v b = B.char7 ',' <> encodeKV k v <> b
{-# INLINE encodeMap #-}

encodeWithKey :: (ToJSON k, ToJSON v) =>
                 ((k -> v -> Series -> Series) -> Series -> m -> Series)
              -> m -> Encoding
encodeWithKey foldrWithKey = brackets '{' '}' . foldrWithKey go mempty
  where go k v c = Value (Encoding $ encodeKV k v) <> c
{-# INLINE encodeWithKey #-}

encodeKV :: (ToJSON k, ToJSON v) => k -> v -> B.Builder
encodeKV k v = builder k <> B.char7 ':' <> builder v
{-# INLINE encodeKV #-}

instance (FromJSON v) => FromJSON (M.Map Text v) where
    parseJSON = withObject "Map Text a" $
                  fmap (H.foldrWithKey M.insert M.empty) . H.traverseWithKey (\k v -> parseJSON v <?> Key k)

instance (ToJSON v) => ToJSON (M.Map LT.Text v) where
    toJSON = Object . mapHashKeyVal LT.toStrict toJSON
    {-# INLINE toJSON #-}

    toEncoding = encodeMap M.minViewWithKey M.foldrWithKey
    {-# INLINE toEncoding #-}

instance (FromJSON v) => FromJSON (M.Map LT.Text v) where
    parseJSON = fmap (hashMapKey LT.fromStrict) . parseJSON
    {-# INLINE parseJSON #-}

instance (ToJSON v) => ToJSON (M.Map String v) where
    toJSON = Object . mapHashKeyVal pack toJSON
    {-# INLINE toJSON #-}

    toEncoding = encodeMap M.minViewWithKey M.foldrWithKey
    {-# INLINE toEncoding #-}

instance (FromJSON v) => FromJSON (M.Map String v) where
    parseJSON = fmap (hashMapKey unpack) . parseJSON
    {-# INLINE parseJSON #-}

instance (ToJSON v) => ToJSON (H.HashMap Text v) where
    toJSON = Object . H.map toJSON
    {-# INLINE toJSON #-}

    toEncoding = encodeWithKey H.foldrWithKey
    {-# INLINE toEncoding #-}

instance (FromJSON v) => FromJSON (H.HashMap Text v) where
    parseJSON = withObject "HashMap Text a" $ H.traverseWithKey (\k v -> parseJSON v <?> Key k)
    {-# INLINE parseJSON #-}

instance (ToJSON v) => ToJSON (H.HashMap LT.Text v) where
    toJSON = Object . mapKeyVal LT.toStrict toJSON
    {-# INLINE toJSON #-}

    toEncoding = encodeWithKey H.foldrWithKey
    {-# INLINE toEncoding #-}

instance (FromJSON v) => FromJSON (H.HashMap LT.Text v) where
    parseJSON = fmap (mapKey LT.fromStrict) . parseJSON
    {-# INLINE parseJSON #-}

instance (ToJSON v) => ToJSON (H.HashMap String v) where
    toJSON = Object . mapKeyVal pack toJSON
    {-# INLINE toJSON #-}

    toEncoding = encodeWithKey H.foldrWithKey
    {-# INLINE toEncoding #-}

instance (FromJSON v) => FromJSON (H.HashMap String v) where
    parseJSON = fmap (mapKey unpack) . parseJSON
    {-# INLINE parseJSON #-}

instance (ToJSON v) => ToJSON (Tree.Tree v) where
    toJSON (Tree.Node root branches) = toJSON (root,branches)
    {-# INLINE toJSON #-}

    toEncoding (Tree.Node root branches) = toEncoding (root,branches)
    {-# INLINE toEncoding #-}

instance (FromJSON v) => FromJSON (Tree.Tree v) where
    parseJSON j = uncurry Tree.Node <$> parseJSON j
    {-# INLINE parseJSON #-}

instance ToJSON Value where
    toJSON a = a
    {-# INLINE toJSON #-}

    toEncoding = Encoding . E.encodeToBuilder
    {-# INLINE toEncoding #-}

instance FromJSON Value where
    parseJSON a = pure a
    {-# INLINE parseJSON #-}

instance ToJSON DotNetTime where
    toJSON = toJSON . dotNetTime

    toEncoding = toEncoding . dotNetTime

dotNetTime :: DotNetTime -> String
dotNetTime (DotNetTime t) = secs ++ formatMillis t ++ ")/"
  where secs  = formatTime defaultTimeLocale "/Date(%s" t

instance FromJSON DotNetTime where
    parseJSON = withText "DotNetTime" $ \t ->
        let (s,m) = T.splitAt (T.length t - 5) t
            t'    = T.concat [s,".",m]
        in case parseTime defaultTimeLocale "/Date(%s%Q)/" (unpack t') of
             Just d -> pure (DotNetTime d)
             _      -> fail "could not parse .NET time"
    {-# INLINE parseJSON #-}

instance ToJSON Day where
    toJSON       = stringEncoding
    toEncoding z = Encoding (E.quote $ E.day z)

instance FromJSON Day where
    parseJSON = withText "Day" (Time.run Time.day)

instance ToJSON TimeOfDay where
    toJSON       = stringEncoding
    toEncoding z = Encoding (E.quote $ E.timeOfDay z)

instance FromJSON TimeOfDay where
    parseJSON = withText "TimeOfDay" (Time.run Time.timeOfDay)

instance ToJSON LocalTime where
    toJSON       = stringEncoding
    toEncoding z = Encoding (E.quote $ E.localTime z)

instance FromJSON LocalTime where
    parseJSON = withText "LocalTime" (Time.run Time.localTime)

instance ToJSON ZonedTime where
    toJSON = stringEncoding

    toEncoding z = Encoding (E.quote $ E.zonedTime z)

formatMillis :: (FormatTime t) => t -> String
formatMillis = take 3 . formatTime defaultTimeLocale "%q"

instance FromJSON ZonedTime where
    parseJSON = withText "ZonedTime" (Time.run Time.zonedTime)

instance ToJSON UTCTime where
    toJSON = stringEncoding

    toEncoding t = Encoding (E.quote $ E.utcTime t)

-- | Encode something to a JSON string.
stringEncoding :: (ToJSON a) => a -> Value
stringEncoding = String . T.dropAround (== '"') . T.decodeLatin1 . L.toStrict . encode
{-# INLINE stringEncoding #-}

instance FromJSON UTCTime where
    parseJSON = withText "UTCTime" (Time.run Time.utcTime)

instance ToJSON NominalDiffTime where
    toJSON = Number . realToFrac
    {-# INLINE toJSON #-}

    toEncoding = Encoding . E.number . realToFrac
    {-# INLINE toEncoding #-}

-- | /WARNING:/ Only parse lengths of time from trusted input
-- since an attacker could easily fill up the memory of the target
-- system by specifying a scientific number with a big exponent like
-- @1e1000000000@.
instance FromJSON NominalDiffTime where
    parseJSON = withScientific "NominalDiffTime" $ pure . realToFrac
    {-# INLINE parseJSON #-}


instance ToJSON1 Dual where
    liftToJSON to = to . getDual
    {-# INLINE liftToJSON #-}

    liftToEncoding to = to . getDual
    {-# INLINE liftToEncoding #-}

instance ToJSON a => ToJSON (Dual a) where
    toJSON = toJSON1
    {-# INLINE toJSON #-}

    toEncoding = toEncoding1
    {-# INLINE toEncoding #-}

instance FromJSON1 Dual where
    liftParseJSON p = fmap Dual . p
    {-# INLINE liftParseJSON #-}

instance FromJSON a => FromJSON (Dual a) where
    parseJSON = parseJSON1
    {-# INLINE parseJSON #-}


instance ToJSON1 First where
    liftToJSON to = liftToJSON to . getFirst
    {-# INLINE liftToJSON #-}

    liftToEncoding to = liftToEncoding to . getFirst
    {-# INLINE liftToEncoding #-}

instance ToJSON a => ToJSON (First a) where
    toJSON = toJSON1
    {-# INLINE toJSON #-}

    toEncoding = toEncoding1
    {-# INLINE toEncoding #-}

instance FromJSON1 First where
    liftParseJSON p = fmap First . liftParseJSON p
    {-# INLINE liftParseJSON #-}

instance FromJSON a => FromJSON (First a) where
    parseJSON = parseJSON1
    {-# INLINE parseJSON #-}


instance ToJSON1 Last where
    liftToJSON to = liftToJSON to . getLast
    {-# INLINE liftToJSON #-}

    liftToEncoding to = liftToEncoding to . getLast
    {-# INLINE liftToEncoding #-}

instance ToJSON a => ToJSON (Last a) where
    toJSON = toJSON1
    {-# INLINE toJSON #-}

    toEncoding = toEncoding1
    {-# INLINE toEncoding #-}

instance FromJSON1 Last where
    liftParseJSON p = fmap Last . liftParseJSON p
    {-# INLINE liftParseJSON #-}

instance FromJSON a => FromJSON (Last a) where
    parseJSON = parseJSON1
    {-# INLINE parseJSON #-}


instance ToJSON Version where
    toJSON = toJSON . showVersion
    {-# INLINE toJSON #-}

    toEncoding = toEncoding . showVersion
    {-# INLINE toEncoding #-}

instance FromJSON Version where
    {-# INLINE parseJSON #-}
    parseJSON = withText "Version" $ go . readP_to_S parseVersion . unpack
      where
        go [(v,[])] = return v
        go (_ : xs) = go xs
        go _        = fail $ "could not parse Version"

instance ToJSON (Proxy a) where
    toJSON _ = Null
    {-# INLINE toJSON #-}

    toEncoding _ = Encoding E.null_
    {-# INLINE toEncoding #-}

instance FromJSON (Proxy a) where
    {-# INLINE parseJSON #-}
    parseJSON Null = pure Proxy
    parseJSON v    = typeMismatch "Proxy" v

instance ToJSON1 (Tagged a) where
    liftToJSON to (Tagged x) = to x
    {-# INLINE liftToJSON #-}

    liftToEncoding to (Tagged x) = to x
    {-# INLINE liftToEncoding #-}

instance ToJSON b => ToJSON (Tagged a b) where
    toJSON = toJSON1
    {-# INLINE toJSON #-}

    toEncoding = toEncoding1
    {-# INLINE toEncoding #-}

instance FromJSON1 (Tagged a) where
    liftParseJSON p = fmap Tagged . p
    {-# INLINE liftParseJSON #-}

instance FromJSON b => FromJSON (Tagged a b) where
    parseJSON = parseJSON1
    {-# INLINE parseJSON #-}

instance ToJSON a => ToJSON (Const a b) where
    toJSON (Const x) = toJSON x
    {-# INLINE toJSON #-}

    toEncoding (Const x) = toEncoding x
    {-# INLINE toEncoding #-}

instance FromJSON a => FromJSON (Const a b) where
    {-# INLINE parseJSON #-}
    parseJSON = fmap Const . parseJSON

-- | @withObject expected f value@ applies @f@ to the 'Object' when @value@ is an @Object@
--   and fails using @'typeMismatch' expected@ otherwise.
withObject :: String -> (Object -> Parser a) -> Value -> Parser a
withObject _        f (Object obj) = f obj
withObject expected _ v            = typeMismatch expected v
{-# INLINE withObject #-}

-- | @withText expected f value@ applies @f@ to the 'Text' when @value@ is a @String@
--   and fails using @'typeMismatch' expected@ otherwise.
withText :: String -> (Text -> Parser a) -> Value -> Parser a
withText _        f (String txt) = f txt
withText expected _ v            = typeMismatch expected v
{-# INLINE withText #-}

-- | @withArray expected f value@ applies @f@ to the 'Array' when @value@ is an @Array@
--   and fails using @'typeMismatch' expected@ otherwise.
withArray :: String -> (Array -> Parser a) -> Value -> Parser a
withArray _        f (Array arr) = f arr
withArray expected _ v           = typeMismatch expected v
{-# INLINE withArray #-}

-- | @withNumber expected f value@ applies @f@ to the 'Number' when @value@ is a 'Number'.
--   and fails using @'typeMismatch' expected@ otherwise.
withNumber :: String -> (Number -> Parser a) -> Value -> Parser a
withNumber expected f = withScientific expected (f . scientificToNumber)
{-# INLINE withNumber #-}
{-# DEPRECATED withNumber "Use withScientific instead" #-}

-- | @withScientific expected f value@ applies @f@ to the 'Scientific' number when @value@ is a 'Number'.
--   and fails using @'typeMismatch' expected@ otherwise.
withScientific :: String -> (Scientific -> Parser a) -> Value -> Parser a
withScientific _        f (Number scientific) = f scientific
withScientific expected _ v                   = typeMismatch expected v
{-# INLINE withScientific #-}

-- | @withBool expected f value@ applies @f@ to the 'Bool' when @value@ is a @Bool@
--   and fails using @'typeMismatch' expected@ otherwise.
withBool :: String -> (Bool -> Parser a) -> Value -> Parser a
withBool _        f (Bool arr) = f arr
withBool expected _ v          = typeMismatch expected v
{-# INLINE withBool #-}

instance KeyValue Pair where
    name .= value = (name, toJSON value)
    {-# INLINE (.=) #-}

instance KeyValue Series where
    name .= value = Value . Encoding $
                    E.text name <> B.char7 ':' <> builder value
    {-# INLINE (.=) #-}

-- | Convert a value from JSON, failing if the types do not match.
fromJSON :: (FromJSON a) => Value -> Result a
fromJSON = parse parseJSON
{-# INLINE fromJSON #-}

-- | Convert a value from JSON, failing if the types do not match.
ifromJSON :: (FromJSON a) => Value -> IResult a
ifromJSON = iparse parseJSON
{-# INLINE ifromJSON #-}

-- | Retrieve the value associated with the given key of an 'Object'.
-- The result is 'empty' if the key is not present or the value cannot
-- be converted to the desired type.
--
-- This accessor is appropriate if the key and value /must/ be present
-- in an object for it to be valid.  If the key and value are
-- optional, use '.:?' instead.
(.:) :: (FromJSON a) => Object -> Text -> Parser a
obj .: key = case H.lookup key obj of
               Nothing -> fail $ "key " ++ show key ++ " not present"
               Just v  -> modifyFailure addKeyName
                        $ parseJSON v <?> Key key
  where
    addKeyName = (("failed to parse field " <> unpack key <> ": ") <>)
{-# INLINE (.:) #-}

-- | Retrieve the value associated with the given key of an 'Object'.
-- The result is 'Nothing' if the key is not present, or 'empty' if
-- the value cannot be converted to the desired type.
--
-- This accessor is most useful if the key and value can be absent
-- from an object without affecting its validity.  If the key and
-- value are mandatory, use '.:' instead.
(.:?) :: (FromJSON a) => Object -> Text -> Parser (Maybe a)
obj .:? key = case H.lookup key obj of
               Nothing -> pure Nothing
               Just v  -> modifyFailure addKeyName
                        $ parseJSON v <?> Key key
  where
    addKeyName = (("failed to parse field " <> unpack key <> ": ") <>)
{-# INLINE (.:?) #-}

-- | Like '.:?', but the resulting parser will fail,
-- if the key is present but is 'Null'.
(.:!) :: (FromJSON a) => Object -> Text -> Parser (Maybe a)
obj .:! key = case H.lookup key obj of
               Nothing -> pure Nothing
               Just v  -> modifyFailure addKeyName
                        $ Just <$> parseJSON v <?> Key key
  where
    addKeyName = (("failed to parse field " <> unpack key <> ": ") <>)
{-# INLINE (.:!) #-}

-- | Helper for use in combination with '.:?' to provide default
-- values for optional JSON object fields.
--
-- This combinator is most useful if the key and value can be absent
-- from an object without affecting its validity and we know a default
-- value to assign in that case.  If the key and value are mandatory,
-- use '.:' instead.
--
-- Example usage:
--
-- @ v1 <- o '.:?' \"opt_field_with_dfl\" .!= \"default_val\"
-- v2 <- o '.:'  \"mandatory_field\"
-- v3 <- o '.:?' \"opt_field2\"
-- @
(.!=) :: Parser (Maybe a) -> a -> Parser a
pmval .!= val = fromMaybe val <$> pmval
{-# INLINE (.!=) #-}

realFloatToJSON :: RealFloat a => a -> Value
realFloatToJSON d
    | isNaN d || isInfinite d = Null
    | otherwise = Number $ Scientific.fromFloatDigits d
{-# INLINE realFloatToJSON #-}

realFloatToEncoding :: RealFloat a => a -> Encoding
realFloatToEncoding d
    | isNaN d || isInfinite d = Encoding E.null_
    | otherwise               = toEncoding (Scientific.fromFloatDigits d)
{-# INLINE realFloatToEncoding #-}

scientificToNumber :: Scientific -> Number
scientificToNumber s
    | e < 0     = D $ Scientific.toRealFloat s
    | otherwise = I $ c * 10 ^ e
  where
    e = Scientific.base10Exponent s
    c = Scientific.coefficient s
{-# INLINE scientificToNumber #-}

parseRealFloat :: RealFloat a => String -> Value -> Parser a
parseRealFloat _        (Number s) = pure $ Scientific.toRealFloat s
parseRealFloat _        Null       = pure (0/0)
parseRealFloat expected v          = typeMismatch expected v
{-# INLINE parseRealFloat #-}

parseIntegral :: Integral a => String -> Value -> Parser a
parseIntegral expected = withScientific expected $ pure . truncate
{-# INLINE parseIntegral #-}
