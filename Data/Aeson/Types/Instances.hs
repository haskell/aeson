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
import qualified Data.Vector.Mutable as VM (unsafeNew, unsafeWrite)
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
parseIndexedJSON idx value = parseJSON value <?> Index idx

instance (ToJSON a) => ToJSON (Identity a) where
    toJSON (Identity a) = toJSON a
    {-# INLINE toJSON #-}

    toEncoding (Identity a) = toEncoding a
    {-# INLINE toEncoding #-}

instance (FromJSON a) => FromJSON (Identity a) where
    parseJSON a      = Identity <$> parseJSON a
    {-# INLINE parseJSON #-}

instance (ToJSON a) => ToJSON (Maybe a) where
    toJSON (Just a) = toJSON a
    toJSON Nothing  = Null
    {-# INLINE toJSON #-}

    toEncoding (Just a) = toEncoding a
    toEncoding Nothing  = Encoding E.null_
    {-# INLINE toEncoding #-}

instance (FromJSON a) => FromJSON (Maybe a) where
    parseJSON Null   = pure Nothing
    parseJSON a      = Just <$> parseJSON a
    {-# INLINE parseJSON #-}

instance (ToJSON a, ToJSON b) => ToJSON (Either a b) where
    toJSON (Left a)  = object [left  .= a]
    toJSON (Right b) = object [right .= b]
    {-# INLINE toJSON #-}

    toEncoding (Left a) = Encoding $
      B.shortByteString "{\"Left\":" <> builder a <> B.char7 '}'
    toEncoding (Right a) = Encoding $
      B.shortByteString "{\"Right\":" <> builder a <> B.char7 '}'
    {-# INLINE toEncoding #-}

instance (FromJSON a, FromJSON b) => FromJSON (Either a b) where
    parseJSON (Object (H.toList -> [(key, value)]))
        | key == left  = Left  <$> parseJSON value <?> Key left
        | key == right = Right <$> parseJSON value <?> Key right
    parseJSON _        = fail $
        "expected an object with a single property " ++
        "where the property key should be either " ++
        "\"Left\" or \"Right\""
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

instance (ToJSON a) => ToJSON (NonEmpty a) where
    toJSON = toJSON . toList
    {-# INLINE toJSON #-}

    toEncoding = toEncoding . toList
    {-# INLINE toEncoding #-}

instance (FromJSON a) => FromJSON (NonEmpty a) where
    parseJSON = withArray "NonEmpty a" $
        (>>= ne) . Tr.sequence . zipWith parseIndexedJSON [0..] . V.toList
      where
        ne []     = fail "Expected a NonEmpty but got an empty list"
        ne (x:xs) = pure (x :| xs)

instance OVERLAPPABLE_ (ToJSON a) => ToJSON [a] where
    toJSON = Array . V.fromList . map toJSON
    {-# INLINE toJSON #-}

    toEncoding xs = list xs
    {-# INLINE toEncoding #-}

instance OVERLAPPABLE_ (FromJSON a) => FromJSON [a] where
    parseJSON = withArray "[a]" $ Tr.sequence .
                zipWith parseIndexedJSON [0..] . V.toList
    {-# INLINE parseJSON #-}

instance (ToJSON a) => ToJSON (Seq.Seq a) where
    toJSON = toJSON . toList
    {-# INLINE toJSON #-}

    toEncoding = foldable
    {-# INLINE toEncoding #-}

instance (FromJSON a) => FromJSON (Seq.Seq a) where
    parseJSON = withArray "Seq a" $
      fmap Seq.fromList .
      Tr.sequence . zipWith parseIndexedJSON [0..] . V.toList
    {-# INLINE parseJSON #-}

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

instance (FromJSON a) => FromJSON (Vector a) where
    parseJSON = withArray "Vector a" $ V.mapM (uncurry parseIndexedJSON) .
                V.indexed
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

    toEncoding = foldable
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

parseJSONElemAtIndex :: FromJSON a => Int -> Vector Value -> Parser a
parseJSONElemAtIndex idx ary = parseJSON (V.unsafeIndex ary idx) <?> Index idx

tuple :: B.Builder -> Encoding
tuple b = Encoding (B.char7 '[' <> b <> B.char7 ']')
{-# INLINE tuple #-}

(>*<) :: B.Builder -> B.Builder -> B.Builder
a >*< b = a <> B.char7 ',' <> b
{-# INLINE (>*<) #-}
infixr 6 >*<

instance (ToJSON a, ToJSON b) => ToJSON (a,b) where
    toJSON (a,b) = Array $ V.create $ do
                     mv <- VM.unsafeNew 2
                     VM.unsafeWrite mv 0 (toJSON a)
                     VM.unsafeWrite mv 1 (toJSON b)
                     return mv
    {-# INLINE toJSON #-}

    toEncoding (a,b) = tuple $
      builder a >*< builder b
    {-# INLINE toEncoding #-}

instance (FromJSON a, FromJSON b) => FromJSON (a,b) where
    parseJSON = withArray "(a,b)" $ \ab ->
        let n = V.length ab
        in if n == 2
             then (,) <$> parseJSONElemAtIndex 0 ab
                      <*> parseJSONElemAtIndex 1 ab
             else fail $ "cannot unpack array of length " ++
                         show n ++ " into a pair"
    {-# INLINE parseJSON #-}

instance (ToJSON a, ToJSON b, ToJSON c) => ToJSON (a,b,c) where
    toJSON (a,b,c) = Array $ V.create $ do
                       mv <- VM.unsafeNew 3
                       VM.unsafeWrite mv 0 (toJSON a)
                       VM.unsafeWrite mv 1 (toJSON b)
                       VM.unsafeWrite mv 2 (toJSON c)
                       return mv
    {-# INLINE toJSON #-}

    toEncoding (a,b,c) = tuple $
      builder a >*<
      builder b >*<
      builder c
    {-# INLINE toEncoding #-}

instance (FromJSON a, FromJSON b, FromJSON c) => FromJSON (a,b,c) where
    parseJSON = withArray "(a,b,c)" $ \abc ->
        let n = V.length abc
        in if n == 3
             then (,,) <$> parseJSONElemAtIndex 0 abc
                       <*> parseJSONElemAtIndex 1 abc
                       <*> parseJSONElemAtIndex 2 abc
             else fail $ "cannot unpack array of length " ++
                          show n ++ " into a 3-tuple"
    {-# INLINE parseJSON #-}

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d) => ToJSON (a,b,c,d) where
    toJSON (a,b,c,d) = Array $ V.create $ do
                         mv <- VM.unsafeNew 4
                         VM.unsafeWrite mv 0 (toJSON a)
                         VM.unsafeWrite mv 1 (toJSON b)
                         VM.unsafeWrite mv 2 (toJSON c)
                         VM.unsafeWrite mv 3 (toJSON d)
                         return mv
    {-# INLINE toJSON #-}

    toEncoding (a,b,c,d) = tuple $
      builder a >*<
      builder b >*<
      builder c >*<
      builder d
    {-# INLINE toEncoding #-}

instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d) =>
         FromJSON (a,b,c,d) where
    parseJSON = withArray "(a,b,c,d)" $ \abcd ->
        let n = V.length abcd
        in if n == 4
             then (,,,) <$> parseJSONElemAtIndex 0 abcd
                        <*> parseJSONElemAtIndex 1 abcd
                        <*> parseJSONElemAtIndex 2 abcd
                        <*> parseJSONElemAtIndex 3 abcd
             else fail $ "cannot unpack array of length " ++
                         show n ++ " into a 4-tuple"
    {-# INLINE parseJSON #-}

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e) =>
         ToJSON (a,b,c,d,e) where
    toJSON (a,b,c,d,e) = Array $ V.create $ do
                           mv <- VM.unsafeNew 5
                           VM.unsafeWrite mv 0 (toJSON a)
                           VM.unsafeWrite mv 1 (toJSON b)
                           VM.unsafeWrite mv 2 (toJSON c)
                           VM.unsafeWrite mv 3 (toJSON d)
                           VM.unsafeWrite mv 4 (toJSON e)
                           return mv
    {-# INLINE toJSON #-}

    toEncoding (a,b,c,d,e) = tuple $
      builder a >*<
      builder b >*<
      builder c >*<
      builder d >*<
      builder e
    {-# INLINE toEncoding #-}

instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d, FromJSON e) =>
         FromJSON (a,b,c,d,e) where
    parseJSON = withArray "(a,b,c,d,e)" $ \abcde ->
        let n = V.length abcde
        in if n == 5
             then (,,,,) <$> parseJSONElemAtIndex 0 abcde
                         <*> parseJSONElemAtIndex 1 abcde
                         <*> parseJSONElemAtIndex 2 abcde
                         <*> parseJSONElemAtIndex 3 abcde
                         <*> parseJSONElemAtIndex 4 abcde
             else fail $ "cannot unpack array of length " ++
                         show n ++ " into a 5-tuple"
    {-# INLINE parseJSON #-}

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f) =>
         ToJSON (a,b,c,d,e,f) where
    toJSON (a,b,c,d,e,f) = Array $ V.create $ do
                             mv <- VM.unsafeNew 6
                             VM.unsafeWrite mv 0 (toJSON a)
                             VM.unsafeWrite mv 1 (toJSON b)
                             VM.unsafeWrite mv 2 (toJSON c)
                             VM.unsafeWrite mv 3 (toJSON d)
                             VM.unsafeWrite mv 4 (toJSON e)
                             VM.unsafeWrite mv 5 (toJSON f)
                             return mv
    {-# INLINE toJSON #-}

    toEncoding (a,b,c,d,e,f) = tuple $
      builder a >*<
      builder b >*<
      builder c >*<
      builder d >*<
      builder e >*<
      builder f
    {-# INLINE toEncoding #-}

instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d, FromJSON e,
          FromJSON f) => FromJSON (a,b,c,d,e,f) where
    parseJSON = withArray "(a,b,c,d,e,f)" $ \abcdef ->
        let n = V.length abcdef
        in if n == 6
             then (,,,,,) <$> parseJSONElemAtIndex 0 abcdef
                          <*> parseJSONElemAtIndex 1 abcdef
                          <*> parseJSONElemAtIndex 2 abcdef
                          <*> parseJSONElemAtIndex 3 abcdef
                          <*> parseJSONElemAtIndex 4 abcdef
                          <*> parseJSONElemAtIndex 5 abcdef
             else fail $ "cannot unpack array of length " ++
                         show n ++ " into a 6-tuple"
    {-# INLINE parseJSON #-}

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f,
          ToJSON g) => ToJSON (a,b,c,d,e,f,g) where
    toJSON (a,b,c,d,e,f,g) = Array $ V.create $ do
                               mv <- VM.unsafeNew 7
                               VM.unsafeWrite mv 0 (toJSON a)
                               VM.unsafeWrite mv 1 (toJSON b)
                               VM.unsafeWrite mv 2 (toJSON c)
                               VM.unsafeWrite mv 3 (toJSON d)
                               VM.unsafeWrite mv 4 (toJSON e)
                               VM.unsafeWrite mv 5 (toJSON f)
                               VM.unsafeWrite mv 6 (toJSON g)
                               return mv
    {-# INLINE toJSON #-}

    toEncoding (a,b,c,d,e,f,g) = tuple $
      builder a >*<
      builder b >*<
      builder c >*<
      builder d >*<
      builder e >*<
      builder f >*<
      builder g
    {-# INLINE toEncoding #-}

instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d, FromJSON e,
          FromJSON f, FromJSON g) => FromJSON (a,b,c,d,e,f,g) where
    parseJSON = withArray "(a,b,c,d,e,f,g)" $ \abcdefg ->
        let n = V.length abcdefg
        in if n == 7
             then (,,,,,,) <$> parseJSONElemAtIndex 0 abcdefg
                           <*> parseJSONElemAtIndex 1 abcdefg
                           <*> parseJSONElemAtIndex 2 abcdefg
                           <*> parseJSONElemAtIndex 3 abcdefg
                           <*> parseJSONElemAtIndex 4 abcdefg
                           <*> parseJSONElemAtIndex 5 abcdefg
                           <*> parseJSONElemAtIndex 6 abcdefg
             else fail $ "cannot unpack array of length " ++
                         show n ++ " into a 7-tuple"
    {-# INLINE parseJSON #-}

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f,
          ToJSON g, ToJSON h) => ToJSON (a,b,c,d,e,f,g,h) where
    toJSON (a,b,c,d,e,f,g,h) = Array $ V.create $ do
      mv <- VM.unsafeNew 8
      VM.unsafeWrite mv 0 (toJSON a)
      VM.unsafeWrite mv 1 (toJSON b)
      VM.unsafeWrite mv 2 (toJSON c)
      VM.unsafeWrite mv 3 (toJSON d)
      VM.unsafeWrite mv 4 (toJSON e)
      VM.unsafeWrite mv 5 (toJSON f)
      VM.unsafeWrite mv 6 (toJSON g)
      VM.unsafeWrite mv 7 (toJSON h)
      return mv
    {-# INLINE toJSON #-}

    toEncoding (a,b,c,d,e,f,g,h) = tuple $
      builder a >*<
      builder b >*<
      builder c >*<
      builder d >*<
      builder e >*<
      builder f >*<
      builder g >*<
      builder h
    {-# INLINE toEncoding #-}

instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d, FromJSON e,
          FromJSON f, FromJSON g, FromJSON h) =>
         FromJSON (a,b,c,d,e,f,g,h) where
    parseJSON = withArray "(a,b,c,d,e,f,g,h)" $ \ary ->
        let n = V.length ary
        in if n /= 8
           then fail $ "cannot unpack array of length " ++
                       show n ++ " into an 8-tuple"
           else (,,,,,,,)
                <$> parseJSONElemAtIndex 0 ary
                <*> parseJSONElemAtIndex 1 ary
                <*> parseJSONElemAtIndex 2 ary
                <*> parseJSONElemAtIndex 3 ary
                <*> parseJSONElemAtIndex 4 ary
                <*> parseJSONElemAtIndex 5 ary
                <*> parseJSONElemAtIndex 6 ary
                <*> parseJSONElemAtIndex 7 ary
    {-# INLINE parseJSON #-}

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f,
          ToJSON g, ToJSON h, ToJSON i) => ToJSON (a,b,c,d,e,f,g,h,i) where
    toJSON (a,b,c,d,e,f,g,h,i) = Array $ V.create $ do
      mv <- VM.unsafeNew 9
      VM.unsafeWrite mv 0 (toJSON a)
      VM.unsafeWrite mv 1 (toJSON b)
      VM.unsafeWrite mv 2 (toJSON c)
      VM.unsafeWrite mv 3 (toJSON d)
      VM.unsafeWrite mv 4 (toJSON e)
      VM.unsafeWrite mv 5 (toJSON f)
      VM.unsafeWrite mv 6 (toJSON g)
      VM.unsafeWrite mv 7 (toJSON h)
      VM.unsafeWrite mv 8 (toJSON i)
      return mv
    {-# INLINE toJSON #-}

    toEncoding (a,b,c,d,e,f,g,h,i) = tuple $
      builder a >*<
      builder b >*<
      builder c >*<
      builder d >*<
      builder e >*<
      builder f >*<
      builder g >*<
      builder h >*<
      builder i
    {-# INLINE toEncoding #-}

instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d, FromJSON e,
          FromJSON f, FromJSON g, FromJSON h, FromJSON i) =>
         FromJSON (a,b,c,d,e,f,g,h,i) where
    parseJSON = withArray "(a,b,c,d,e,f,g,h,i)" $ \ary ->
        let n = V.length ary
        in if n /= 9
           then fail $ "cannot unpack array of length " ++
                       show n ++ " into a 9-tuple"
           else (,,,,,,,,)
                <$> parseJSONElemAtIndex 0 ary
                <*> parseJSONElemAtIndex 1 ary
                <*> parseJSONElemAtIndex 2 ary
                <*> parseJSONElemAtIndex 3 ary
                <*> parseJSONElemAtIndex 4 ary
                <*> parseJSONElemAtIndex 5 ary
                <*> parseJSONElemAtIndex 6 ary
                <*> parseJSONElemAtIndex 7 ary
                <*> parseJSONElemAtIndex 8 ary
    {-# INLINE parseJSON #-}

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f,
          ToJSON g, ToJSON h, ToJSON i, ToJSON j) =>
         ToJSON (a,b,c,d,e,f,g,h,i,j) where
    toJSON (a,b,c,d,e,f,g,h,i,j) = Array $ V.create $ do
      mv <- VM.unsafeNew 10
      VM.unsafeWrite mv 0 (toJSON a)
      VM.unsafeWrite mv 1 (toJSON b)
      VM.unsafeWrite mv 2 (toJSON c)
      VM.unsafeWrite mv 3 (toJSON d)
      VM.unsafeWrite mv 4 (toJSON e)
      VM.unsafeWrite mv 5 (toJSON f)
      VM.unsafeWrite mv 6 (toJSON g)
      VM.unsafeWrite mv 7 (toJSON h)
      VM.unsafeWrite mv 8 (toJSON i)
      VM.unsafeWrite mv 9 (toJSON j)
      return mv
    {-# INLINE toJSON #-}

    toEncoding (a,b,c,d,e,f,g,h,i,j) = tuple $
      builder a >*<
      builder b >*<
      builder c >*<
      builder d >*<
      builder e >*<
      builder f >*<
      builder g >*<
      builder h >*<
      builder i >*<
      builder j
    {-# INLINE toEncoding #-}

instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d, FromJSON e,
          FromJSON f, FromJSON g, FromJSON h, FromJSON i, FromJSON j) =>
         FromJSON (a,b,c,d,e,f,g,h,i,j) where
    parseJSON = withArray "(a,b,c,d,e,f,g,h,i,j)" $ \ary ->
        let n = V.length ary
        in if n /= 10
           then fail $ "cannot unpack array of length " ++
                       show n ++ " into a 10-tuple"
           else (,,,,,,,,,)
                <$> parseJSONElemAtIndex 0 ary
                <*> parseJSONElemAtIndex 1 ary
                <*> parseJSONElemAtIndex 2 ary
                <*> parseJSONElemAtIndex 3 ary
                <*> parseJSONElemAtIndex 4 ary
                <*> parseJSONElemAtIndex 5 ary
                <*> parseJSONElemAtIndex 6 ary
                <*> parseJSONElemAtIndex 7 ary
                <*> parseJSONElemAtIndex 8 ary
                <*> parseJSONElemAtIndex 9 ary
    {-# INLINE parseJSON #-}

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f,
          ToJSON g, ToJSON h, ToJSON i, ToJSON j, ToJSON k) =>
         ToJSON (a,b,c,d,e,f,g,h,i,j,k) where
    toJSON (a,b,c,d,e,f,g,h,i,j,k) = Array $ V.create $ do
      mv <- VM.unsafeNew 11
      VM.unsafeWrite mv 0 (toJSON a)
      VM.unsafeWrite mv 1 (toJSON b)
      VM.unsafeWrite mv 2 (toJSON c)
      VM.unsafeWrite mv 3 (toJSON d)
      VM.unsafeWrite mv 4 (toJSON e)
      VM.unsafeWrite mv 5 (toJSON f)
      VM.unsafeWrite mv 6 (toJSON g)
      VM.unsafeWrite mv 7 (toJSON h)
      VM.unsafeWrite mv 8 (toJSON i)
      VM.unsafeWrite mv 9 (toJSON j)
      VM.unsafeWrite mv 10 (toJSON k)
      return mv
    {-# INLINE toJSON #-}

    toEncoding (a,b,c,d,e,f,g,h,i,j,k) = tuple $
      builder a >*<
      builder b >*<
      builder c >*<
      builder d >*<
      builder e >*<
      builder f >*<
      builder g >*<
      builder h >*<
      builder i >*<
      builder j >*<
      builder k
    {-# INLINE toEncoding #-}

instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d, FromJSON e,
          FromJSON f, FromJSON g, FromJSON h, FromJSON i, FromJSON j,
          FromJSON k) =>
         FromJSON (a,b,c,d,e,f,g,h,i,j,k) where
    parseJSON = withArray "(a,b,c,d,e,f,g,h,i,j,k)" $ \ary ->
        let n = V.length ary
        in if n /= 11
           then fail $ "cannot unpack array of length " ++
                       show n ++ " into an 11-tuple"
           else (,,,,,,,,,,)
                <$> parseJSONElemAtIndex 0 ary
                <*> parseJSONElemAtIndex 1 ary
                <*> parseJSONElemAtIndex 2 ary
                <*> parseJSONElemAtIndex 3 ary
                <*> parseJSONElemAtIndex 4 ary
                <*> parseJSONElemAtIndex 5 ary
                <*> parseJSONElemAtIndex 6 ary
                <*> parseJSONElemAtIndex 7 ary
                <*> parseJSONElemAtIndex 8 ary
                <*> parseJSONElemAtIndex 9 ary
                <*> parseJSONElemAtIndex 10 ary
    {-# INLINE parseJSON #-}

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f,
          ToJSON g, ToJSON h, ToJSON i, ToJSON j, ToJSON k, ToJSON l) =>
         ToJSON (a,b,c,d,e,f,g,h,i,j,k,l) where
    toJSON (a,b,c,d,e,f,g,h,i,j,k,l) = Array $ V.create $ do
      mv <- VM.unsafeNew 12
      VM.unsafeWrite mv 0 (toJSON a)
      VM.unsafeWrite mv 1 (toJSON b)
      VM.unsafeWrite mv 2 (toJSON c)
      VM.unsafeWrite mv 3 (toJSON d)
      VM.unsafeWrite mv 4 (toJSON e)
      VM.unsafeWrite mv 5 (toJSON f)
      VM.unsafeWrite mv 6 (toJSON g)
      VM.unsafeWrite mv 7 (toJSON h)
      VM.unsafeWrite mv 8 (toJSON i)
      VM.unsafeWrite mv 9 (toJSON j)
      VM.unsafeWrite mv 10 (toJSON k)
      VM.unsafeWrite mv 11 (toJSON l)
      return mv
    {-# INLINE toJSON #-}

    toEncoding (a,b,c,d,e,f,g,h,i,j,k,l) = tuple $
      builder a >*<
      builder b >*<
      builder c >*<
      builder d >*<
      builder e >*<
      builder f >*<
      builder g >*<
      builder h >*<
      builder i >*<
      builder j >*<
      builder k >*<
      builder l
    {-# INLINE toEncoding #-}

instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d, FromJSON e,
          FromJSON f, FromJSON g, FromJSON h, FromJSON i, FromJSON j,
          FromJSON k, FromJSON l) =>
         FromJSON (a,b,c,d,e,f,g,h,i,j,k,l) where
    parseJSON = withArray "(a,b,c,d,e,f,g,h,i,j,k,l)" $ \ary ->
        let n = V.length ary
        in if n /= 12
           then fail $ "cannot unpack array of length " ++
                       show n ++ " into a 12-tuple"
           else (,,,,,,,,,,,)
                <$> parseJSONElemAtIndex 0 ary
                <*> parseJSONElemAtIndex 1 ary
                <*> parseJSONElemAtIndex 2 ary
                <*> parseJSONElemAtIndex 3 ary
                <*> parseJSONElemAtIndex 4 ary
                <*> parseJSONElemAtIndex 5 ary
                <*> parseJSONElemAtIndex 6 ary
                <*> parseJSONElemAtIndex 7 ary
                <*> parseJSONElemAtIndex 8 ary
                <*> parseJSONElemAtIndex 9 ary
                <*> parseJSONElemAtIndex 10 ary
                <*> parseJSONElemAtIndex 11 ary
    {-# INLINE parseJSON #-}

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f,
          ToJSON g, ToJSON h, ToJSON i, ToJSON j, ToJSON k, ToJSON l,
          ToJSON m) =>
         ToJSON (a,b,c,d,e,f,g,h,i,j,k,l,m) where
    toJSON (a,b,c,d,e,f,g,h,i,j,k,l,m) = Array $ V.create $ do
      mv <- VM.unsafeNew 13
      VM.unsafeWrite mv 0 (toJSON a)
      VM.unsafeWrite mv 1 (toJSON b)
      VM.unsafeWrite mv 2 (toJSON c)
      VM.unsafeWrite mv 3 (toJSON d)
      VM.unsafeWrite mv 4 (toJSON e)
      VM.unsafeWrite mv 5 (toJSON f)
      VM.unsafeWrite mv 6 (toJSON g)
      VM.unsafeWrite mv 7 (toJSON h)
      VM.unsafeWrite mv 8 (toJSON i)
      VM.unsafeWrite mv 9 (toJSON j)
      VM.unsafeWrite mv 10 (toJSON k)
      VM.unsafeWrite mv 11 (toJSON l)
      VM.unsafeWrite mv 12 (toJSON m)
      return mv
    {-# INLINE toJSON #-}

    toEncoding (a,b,c,d,e,f,g,h,i,j,k,l,m) = tuple $
      builder a >*<
      builder b >*<
      builder c >*<
      builder d >*<
      builder e >*<
      builder f >*<
      builder g >*<
      builder h >*<
      builder i >*<
      builder j >*<
      builder k >*<
      builder l >*<
      builder m
    {-# INLINE toEncoding #-}

instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d, FromJSON e,
          FromJSON f, FromJSON g, FromJSON h, FromJSON i, FromJSON j,
          FromJSON k, FromJSON l, FromJSON m) =>
         FromJSON (a,b,c,d,e,f,g,h,i,j,k,l,m) where
    parseJSON = withArray "(a,b,c,d,e,f,g,h,i,j,k,l,m)" $ \ary ->
        let n = V.length ary
        in if n /= 13
           then fail $ "cannot unpack array of length " ++
                       show n ++ " into a 13-tuple"
           else (,,,,,,,,,,,,)
                <$> parseJSONElemAtIndex 0 ary
                <*> parseJSONElemAtIndex 1 ary
                <*> parseJSONElemAtIndex 2 ary
                <*> parseJSONElemAtIndex 3 ary
                <*> parseJSONElemAtIndex 4 ary
                <*> parseJSONElemAtIndex 5 ary
                <*> parseJSONElemAtIndex 6 ary
                <*> parseJSONElemAtIndex 7 ary
                <*> parseJSONElemAtIndex 8 ary
                <*> parseJSONElemAtIndex 9 ary
                <*> parseJSONElemAtIndex 10 ary
                <*> parseJSONElemAtIndex 11 ary
                <*> parseJSONElemAtIndex 12 ary
    {-# INLINE parseJSON #-}

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f,
          ToJSON g, ToJSON h, ToJSON i, ToJSON j, ToJSON k, ToJSON l,
          ToJSON m, ToJSON n) =>
         ToJSON (a,b,c,d,e,f,g,h,i,j,k,l,m,n) where
    toJSON (a,b,c,d,e,f,g,h,i,j,k,l,m,n) = Array $ V.create $ do
      mv <- VM.unsafeNew 14
      VM.unsafeWrite mv 0 (toJSON a)
      VM.unsafeWrite mv 1 (toJSON b)
      VM.unsafeWrite mv 2 (toJSON c)
      VM.unsafeWrite mv 3 (toJSON d)
      VM.unsafeWrite mv 4 (toJSON e)
      VM.unsafeWrite mv 5 (toJSON f)
      VM.unsafeWrite mv 6 (toJSON g)
      VM.unsafeWrite mv 7 (toJSON h)
      VM.unsafeWrite mv 8 (toJSON i)
      VM.unsafeWrite mv 9 (toJSON j)
      VM.unsafeWrite mv 10 (toJSON k)
      VM.unsafeWrite mv 11 (toJSON l)
      VM.unsafeWrite mv 12 (toJSON m)
      VM.unsafeWrite mv 13 (toJSON n)
      return mv
    {-# INLINE toJSON #-}

    toEncoding (a,b,c,d,e,f,g,h,i,j,k,l,m,n) = tuple $
      builder a >*<
      builder b >*<
      builder c >*<
      builder d >*<
      builder e >*<
      builder f >*<
      builder g >*<
      builder h >*<
      builder i >*<
      builder j >*<
      builder k >*<
      builder l >*<
      builder m >*<
      builder n
    {-# INLINE toEncoding #-}

instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d, FromJSON e,
          FromJSON f, FromJSON g, FromJSON h, FromJSON i, FromJSON j,
          FromJSON k, FromJSON l, FromJSON m, FromJSON n) =>
         FromJSON (a,b,c,d,e,f,g,h,i,j,k,l,m,n) where
    parseJSON = withArray "(a,b,c,d,e,f,g,h,i,j,k,l,m,n)" $ \ary ->
        let n = V.length ary
        in if n /= 14
           then fail $ "cannot unpack array of length " ++
                       show n ++ " into a 14-tuple"
           else (,,,,,,,,,,,,,)
                <$> parseJSONElemAtIndex 0 ary
                <*> parseJSONElemAtIndex 1 ary
                <*> parseJSONElemAtIndex 2 ary
                <*> parseJSONElemAtIndex 3 ary
                <*> parseJSONElemAtIndex 4 ary
                <*> parseJSONElemAtIndex 5 ary
                <*> parseJSONElemAtIndex 6 ary
                <*> parseJSONElemAtIndex 7 ary
                <*> parseJSONElemAtIndex 8 ary
                <*> parseJSONElemAtIndex 9 ary
                <*> parseJSONElemAtIndex 10 ary
                <*> parseJSONElemAtIndex 11 ary
                <*> parseJSONElemAtIndex 12 ary
                <*> parseJSONElemAtIndex 13 ary
    {-# INLINE parseJSON #-}

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f,
          ToJSON g, ToJSON h, ToJSON i, ToJSON j, ToJSON k, ToJSON l,
          ToJSON m, ToJSON n, ToJSON o) =>
         ToJSON (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) where
    toJSON (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) = Array $ V.create $ do
      mv <- VM.unsafeNew 15
      VM.unsafeWrite mv 0 (toJSON a)
      VM.unsafeWrite mv 1 (toJSON b)
      VM.unsafeWrite mv 2 (toJSON c)
      VM.unsafeWrite mv 3 (toJSON d)
      VM.unsafeWrite mv 4 (toJSON e)
      VM.unsafeWrite mv 5 (toJSON f)
      VM.unsafeWrite mv 6 (toJSON g)
      VM.unsafeWrite mv 7 (toJSON h)
      VM.unsafeWrite mv 8 (toJSON i)
      VM.unsafeWrite mv 9 (toJSON j)
      VM.unsafeWrite mv 10 (toJSON k)
      VM.unsafeWrite mv 11 (toJSON l)
      VM.unsafeWrite mv 12 (toJSON m)
      VM.unsafeWrite mv 13 (toJSON n)
      VM.unsafeWrite mv 14 (toJSON o)
      return mv
    {-# INLINE toJSON #-}

    toEncoding (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) = tuple $
      builder a >*<
      builder b >*<
      builder c >*<
      builder d >*<
      builder e >*<
      builder f >*<
      builder g >*<
      builder h >*<
      builder i >*<
      builder j >*<
      builder k >*<
      builder l >*<
      builder m >*<
      builder n >*<
      builder o
    {-# INLINE toEncoding #-}

instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d, FromJSON e,
          FromJSON f, FromJSON g, FromJSON h, FromJSON i, FromJSON j,
          FromJSON k, FromJSON l, FromJSON m, FromJSON n, FromJSON o) =>
         FromJSON (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) where
    parseJSON = withArray "(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o)" $ \ary ->
        let n = V.length ary
        in if n /= 15
           then fail $ "cannot unpack array of length " ++
                       show n ++ " into a 15-tuple"
           else (,,,,,,,,,,,,,,)
                <$> parseJSONElemAtIndex 0 ary
                <*> parseJSONElemAtIndex 1 ary
                <*> parseJSONElemAtIndex 2 ary
                <*> parseJSONElemAtIndex 3 ary
                <*> parseJSONElemAtIndex 4 ary
                <*> parseJSONElemAtIndex 5 ary
                <*> parseJSONElemAtIndex 6 ary
                <*> parseJSONElemAtIndex 7 ary
                <*> parseJSONElemAtIndex 8 ary
                <*> parseJSONElemAtIndex 9 ary
                <*> parseJSONElemAtIndex 10 ary
                <*> parseJSONElemAtIndex 11 ary
                <*> parseJSONElemAtIndex 12 ary
                <*> parseJSONElemAtIndex 13 ary
                <*> parseJSONElemAtIndex 14 ary
    {-# INLINE parseJSON #-}

instance ToJSON a => ToJSON (Dual a) where
    toJSON = toJSON . getDual
    {-# INLINE toJSON #-}

    toEncoding = toEncoding . getDual
    {-# INLINE toEncoding #-}

instance FromJSON a => FromJSON (Dual a) where
    parseJSON = fmap Dual . parseJSON
    {-# INLINE parseJSON #-}

instance ToJSON a => ToJSON (First a) where
    toJSON = toJSON . getFirst
    {-# INLINE toJSON #-}

    toEncoding = toEncoding . getFirst
    {-# INLINE toEncoding #-}

instance FromJSON a => FromJSON (First a) where
    parseJSON = fmap First . parseJSON
    {-# INLINE parseJSON #-}

instance ToJSON a => ToJSON (Last a) where
    toJSON = toJSON . getLast
    {-# INLINE toJSON #-}

    toEncoding = toEncoding . getLast
    {-# INLINE toEncoding #-}

instance FromJSON a => FromJSON (Last a) where
    parseJSON = fmap Last . parseJSON
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

instance ToJSON b => ToJSON (Tagged a b) where
    toJSON (Tagged x) = toJSON x
    {-# INLINE toJSON #-}

    toEncoding (Tagged x) = toEncoding x
    {-# INLINE toEncoding #-}

instance FromJSON b => FromJSON (Tagged a b) where
    {-# INLINE parseJSON #-}
    parseJSON = fmap Tagged . parseJSON

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
