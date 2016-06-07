{-# LANGUAGE CPP, BangPatterns, DeriveDataTypeable, FlexibleContexts,
    FlexibleInstances, GeneralizedNewtypeDeriving,
    OverloadedStrings, UndecidableInstances, ScopedTypeVariables,
    ViewPatterns #-}
{-# LANGUAGE DefaultSignatures #-}

-- Needed for Tagged, Const and Proxy instances
#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE PolyKinds #-}
#endif

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
    -- ** Keys for maps
    , ToJSONKey(..)
    , ToJSONKeyFunction(..)
    , contramapToJSONKeyFunction
    , FromJSONKey(..)
    , FromJSONKeyFunction(..)
    , fromJSONKeyCoerce
    , coerceFromJSONKeyFunction
    , mapFromJSONKeyFunction
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
    , toEncoding2
    -- ** Generic JSON classes
    , GFromJSON(..)
    , GToJSON(..)
    , GToEncoding(..)
    , Zero
    , One
    , genericToJSON
    , genericLiftToJSON
    , genericToEncoding
    , genericLiftToEncoding
    , genericParseJSON
    , genericLiftParseJSON

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
    , parseOptionalFieldWith
    , (.:!)
    , (.!=)
    , typeMismatch

    , listEncoding
    , listValue
    , listParser
    ) where

import Data.Aeson.Types.Instances.Tuple ()

import Data.Aeson.Encoding.Internal (Encoding (..), emptyArray_, dict, tuple, (>*<))

import Control.Applicative (Const(..))
import Data.Aeson.Encode.Functions (builder)
import Data.Aeson.Functions (mapHashKeyVal, mapKey, mapKeyVal)
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
import Data.Text (Text, unpack)
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
import qualified Data.Aeson.Encode.Builder as EB
import qualified Data.Aeson.Parser.Time as Time
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as L
import qualified Data.HashMap.Strict as H
import qualified Data.HashSet as HashSet
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import qualified Data.Scientific as Scientific
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Read as TR
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as LTB
import qualified Data.Text.Lazy.Builder.Int as LTBI
import qualified Data.Tree as Tree
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Mutable as VM

import Unsafe.Coerce (unsafeCoerce)

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

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

parseIndexedJSON :: (Value -> Parser a) -> Int -> Value -> Parser a
parseIndexedJSON p idx value = p value <?> Index idx
{-# INLINE parseIndexedJSON #-}

parseIndexedJSONPair :: (Value -> Parser a) -> (Value -> Parser b) -> Int -> Value -> Parser (a, b)
parseIndexedJSONPair keyParser valParser idx value = p value <?> Index idx
  where
    p = withArray "(k,v)" $ \ab ->
        let n = V.length ab
        in if n == 2
             then (,) <$> parseJSONElemAtIndex keyParser 0 ab
                      <*> parseJSONElemAtIndex valParser 1 ab
             else fail $ "cannot unpack array of length " ++
                         show n ++ " into a pair"

    parseJSONElemAtIndex :: (Value -> Parser a) -> Int -> Vector Value -> Parser a
    parseJSONElemAtIndex p' idx' ary = p' (V.unsafeIndex ary idx') <?> Index idx'
{-# INLINE parseIndexedJSONPair #-}

toJSONPair :: (a -> Value) -> (b -> Value) -> (a, b) -> Value
toJSONPair keySerialiser valSerializer (a, b) = Array $ V.create $ do
     mv <- VM.unsafeNew 2
     VM.unsafeWrite mv 0 (keySerialiser a)
     VM.unsafeWrite mv 1 (valSerializer b)
     return mv
{-# INLINE toJSONPair #-}

realFloatToJSON :: RealFloat a => a -> Value
realFloatToJSON d
    | isNaN d || isInfinite d = Null
    | otherwise = Number $ Scientific.fromFloatDigits d
{-# INLINE realFloatToJSON #-}

realFloatToEncoding :: RealFloat a => a -> Encoding
realFloatToEncoding d
    | isNaN d || isInfinite d = Encoding EB.null_
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

-------------------------------------------------------------------------------
-- base
-------------------------------------------------------------------------------

instance ToJSON1 Identity where
    liftToJSON to _ (Identity a) = to a
    {-# INLINE liftToJSON #-}

    liftToEncoding to _ (Identity a) = to a
    {-# INLINE liftToEncoding #-}

instance (ToJSON a) => ToJSON (Identity a) where
    toJSON = toJSON1
    {-# INLINE toJSON #-}

    toEncoding = toEncoding1
    {-# INLINE toEncoding #-}

instance FromJSON1 Identity where
    liftParseJSON p _ a = Identity <$> p a
    {-# INLINE liftParseJSON #-}

instance (FromJSON a) => FromJSON (Identity a) where
    parseJSON = parseJSON1
    {-# INLINE parseJSON #-}


instance ToJSON2 Const where
    liftToJSON2 to _ _ _ (Const x) = to x
    {-# INLINE liftToJSON2 #-}

    liftToEncoding2 to _ _ _ (Const x) = to x
    {-# INLINE liftToEncoding2 #-}

instance FromJSON2 Const where
    liftParseJSON2 p _ _ _ = fmap Const . p
    {-# INLINE liftParseJSON2 #-}

instance ToJSON a => ToJSON1 (Const a) where
    liftToJSON _ _ (Const x) = toJSON x
    {-# INLINE liftToJSON #-}

    liftToEncoding _ _ (Const x) = toEncoding x
    {-# INLINE liftToEncoding #-}

instance FromJSON a => FromJSON1 (Const a) where
    liftParseJSON _ _ = fmap Const . parseJSON
    {-# INLINE liftParseJSON #-}

instance ToJSON a => ToJSON (Const a b) where
    toJSON (Const x) = toJSON x
    {-# INLINE toJSON #-}

    toEncoding (Const x) = toEncoding x
    {-# INLINE toEncoding #-}

instance FromJSON a => FromJSON (Const a b) where
    {-# INLINE parseJSON #-}
    parseJSON = fmap Const . parseJSON


instance ToJSON1 Maybe where
    liftToJSON to _ (Just a) = to a
    liftToJSON _  _ Nothing  = Null
    {-# INLINE liftToJSON #-}

    liftToEncoding to _ (Just a) = to a
    liftToEncoding _  _ Nothing  = Encoding EB.null_
    {-# INLINE liftToEncoding #-}

instance (ToJSON a) => ToJSON (Maybe a) where
    toJSON = toJSON1
    {-# INLINE toJSON #-}

    toEncoding = toEncoding1
    {-# INLINE toEncoding #-}

instance FromJSON1 Maybe where
    liftParseJSON _ _ Null = pure Nothing
    liftParseJSON p _ a    = Just <$> p a
    {-# INLINE liftParseJSON #-}

instance (FromJSON a) => FromJSON (Maybe a) where
    parseJSON = parseJSON1
    {-# INLINE parseJSON #-}


instance ToJSON2 Either where
    liftToJSON2  toA _ _toB _ (Left a)  = Object $ H.singleton left  (toA a)
    liftToJSON2 _toA _  toB _ (Right b) = Object $ H.singleton right (toB b)
    {-# INLINE liftToJSON2 #-}

    liftToEncoding2  toA _ _toB _ (Left a) =
        Encoding (B.shortByteString "{\"Left\":")
        <> toA a
        <> Encoding (B.char7 '}')

    liftToEncoding2 _toA _ toB _ (Right b) =
        Encoding (B.shortByteString "{\"Right\":")
        <> toB b
        <> Encoding (B.char7 '}')
    {-# INLINE liftToEncoding2 #-}

instance (ToJSON a) => ToJSON1 (Either a) where
    liftToJSON = liftToJSON2 toJSON toJSONList
    {-# INLINE liftToJSON #-}

    liftToEncoding = liftToEncoding2 toEncoding toEncodingList
    {-# INLINE liftToEncoding #-}

instance (ToJSON a, ToJSON b) => ToJSON (Either a b) where
    toJSON = toJSON2
    {-# INLINE toJSON #-}

    toEncoding = toEncoding2
    {-# INLINE toEncoding #-}

instance FromJSON2 Either where
    liftParseJSON2 pA _ pB _ (Object (H.toList -> [(key, value)]))
        | key == left  = Left  <$> pA value <?> Key left
        | key == right = Right <$> pB value <?> Key right

    liftParseJSON2 _ _ _ _ _ = fail $
        "expected an object with a single property " ++
        "where the property key should be either " ++
        "\"Left\" or \"Right\""
    {-# INLINE liftParseJSON2 #-}

instance (FromJSON a) => FromJSON1 (Either a) where
    liftParseJSON = liftParseJSON2 parseJSON parseJSONList
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

    toEncoding = Encoding . EB.bool
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

    toEncoding _ = emptyArray_
    {-# INLINE toEncoding #-}

instance FromJSON () where
    parseJSON = withArray "()" $ \v ->
                  if V.null v
                    then pure ()
                    else fail "Expected an empty array"
    {-# INLINE parseJSON #-}

instance ToJSON Char where
    toJSON = String . T.singleton
    {-# INLINE toJSON #-}

    toJSONList = String . T.pack
    {-# INLINE toJSONList #-}

    toEncoding = Encoding . EB.string . (:[])
    {-# INLINE toEncoding #-}

    toEncodingList = Encoding . EB.string
    {-# INLINE toEncodingList #-}

instance FromJSON Char where
    parseJSON = withText "Char" $ \t ->
                  if T.compareLength t 1 == EQ
                    then pure $ T.head t
                    else fail "Expected a string of length 1"
    {-# INLINE parseJSON #-}

    parseJSONList = withText "String" $ pure . T.unpack
    {-# INLINE parseJSONList #-}

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

    toEncoding = Encoding . EB.number . realToFrac
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

    toEncoding = Encoding . EB.text
    {-# INLINE toEncoding #-}

instance FromJSON Text where
    parseJSON = withText "Text" pure
    {-# INLINE parseJSON #-}

instance ToJSON LT.Text where
    toJSON = String . LT.toStrict
    {-# INLINE toJSON #-}

    toEncoding t = Encoding $
      B.char7 '"' <>
      LT.foldrChunks (\x xs -> EB.unquoted x <> xs) (B.char7 '"') t
    {-# INLINE toEncoding #-}

instance FromJSON LT.Text where
    parseJSON = withText "Lazy Text" $ pure . LT.fromStrict
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

-------------------------------------------------------------------------------
-- semigroups NonEmpty
-------------------------------------------------------------------------------

instance ToJSON1 NonEmpty where
    liftToJSON to _ = listValue to . NE.toList
    {-# INLINE liftToJSON #-}

    liftToEncoding to _ = listEncoding to . NE.toList
    {-# INLINE liftToEncoding #-}

instance (ToJSON a) => ToJSON (NonEmpty a) where
    toJSON = toJSON1
    {-# INLINE toJSON #-}

    toEncoding = toEncoding1
    {-# INLINE toEncoding #-}

instance FromJSON1 NonEmpty where
    liftParseJSON p _ = withArray "NonEmpty a" $
        (>>= ne) . Tr.sequence . zipWith (parseIndexedJSON p) [0..] . V.toList
      where
        ne []     = fail "Expected a NonEmpty but got an empty list"
        ne (x:xs) = pure (x :| xs)
    {-# INLINE liftParseJSON #-}

instance (FromJSON a) => FromJSON (NonEmpty a) where
    parseJSON = parseJSON1
    {-# INLINE parseJSON #-}

-------------------------------------------------------------------------------
-- scientific
-------------------------------------------------------------------------------

instance ToJSON Scientific where
    toJSON = Number
    {-# INLINE toJSON #-}

    toEncoding = Encoding . EB.number
    {-# INLINE toEncoding #-}

instance FromJSON Scientific where
    parseJSON = withScientific "Scientific" pure
    {-# INLINE parseJSON #-}

-------------------------------------------------------------------------------
-- containers
-------------------------------------------------------------------------------

instance ToJSON1 Seq.Seq where
    liftToJSON to _ = listValue to . toList
    {-# INLINE liftToJSON #-}

    liftToEncoding to _ = listEncoding to . toList
    {-# INLINE liftToEncoding #-}

instance (ToJSON a) => ToJSON (Seq.Seq a) where
    toJSON = toJSON1
    {-# INLINE toJSON #-}

    toEncoding = toEncoding1
    {-# INLINE toEncoding #-}

instance FromJSON1 Seq.Seq where
    liftParseJSON p _ = withArray "Seq a" $
      fmap Seq.fromList .
      Tr.sequence . zipWith (parseIndexedJSON p) [0..] . V.toList
    {-# INLINE liftParseJSON #-}

instance (FromJSON a) => FromJSON (Seq.Seq a) where
    parseJSON = parseJSON1
    {-# INLINE parseJSON #-}


instance ToJSON1 Set.Set where
    liftToJSON to _ = listValue to . Set.toList
    {-# INLINE liftToJSON #-}

    liftToEncoding to _ = listEncoding to . Set.toList
    {-# INLINE liftToEncoding #-}

instance (ToJSON a) => ToJSON (Set.Set a) where
    toJSON = toJSON1
    {-# INLINE toJSON #-}

    toEncoding = toEncoding1
    {-# INLINE toEncoding #-}

instance (Ord a, FromJSON a) => FromJSON (Set.Set a) where
    parseJSON = fmap Set.fromList . parseJSON
    {-# INLINE parseJSON #-}


instance FromJSON IntSet.IntSet where
    parseJSON = fmap IntSet.fromList . parseJSON
    {-# INLINE parseJSON #-}

instance ToJSON IntSet.IntSet where
    toJSON = toJSON . IntSet.toList
    {-# INLINE toJSON #-}

    toEncoding = toEncoding . IntSet.toList
    {-# INLINE toEncoding #-}


instance ToJSON1 IntMap.IntMap where
    liftToJSON to tol = liftToJSON to' tol' . IntMap.toList
      where
        to'  = liftToJSON2     toJSON toJSONList to tol
        tol' = liftToJSONList2 toJSON toJSONList to tol
    {-# INLINE liftToJSON #-}

    liftToEncoding to tol = liftToEncoding to' tol' . IntMap.toList
      where
        to'  = liftToEncoding2     toEncoding toEncodingList to tol
        tol' = liftToEncodingList2 toEncoding toEncodingList to tol
    {-# INLINE liftToEncoding #-}

instance ToJSON a => ToJSON (IntMap.IntMap a) where
    toJSON = toJSON1
    {-# INLINE toJSON #-}

    toEncoding = toEncoding1
    {-# INLINE toEncoding #-}

instance FromJSON1 IntMap.IntMap where
    liftParseJSON p pl = fmap IntMap.fromList . liftParseJSON p' pl'
      where
        p'  = liftParseJSON2     parseJSON parseJSONList p pl
        pl' = liftParseJSONList2 parseJSON parseJSONList p pl
    {-# INLINE liftParseJSON #-}

instance FromJSON a => FromJSON (IntMap.IntMap a) where
    parseJSON = fmap IntMap.fromList . parseJSON
    {-# INLINE parseJSON #-}


instance ToJSONKey k => ToJSON1 (M.Map k) where
    liftToJSON g _ = case toJSONKey of
        ToJSONKeyText (f,_) -> Object . mapHashKeyVal f g
        ToJSONKeyValue (f,_) -> Array . V.fromList . map (toJSONPair f g) . M.toList
    {-# INLINE liftToJSON #-}

    -- liftToEncoding :: forall a. (a -> Encoding) -> ([a] -> Encoding) -> M.Map k a -> Encoding
    liftToEncoding g _ = case toJSONKey of
        ToJSONKeyText (_,f) -> dict f g M.foldrWithKey
        ToJSONKeyValue (_,f) -> listEncoding (pairEncoding f) . M.toList
      where
        -- pairEncoding :: (k -> Encoding) -> (k, a) -> Encoding
        pairEncoding f (a, b) = tuple $ f a >*< g b
    {-# INLINE liftToEncoding #-}

instance (FromJSONKey k, Ord k) => FromJSON1 (M.Map k) where
    liftParseJSON p _ = case fromJSONKey of
        FromJSONKeyCoerce _-> withObject "Map k v" $
            fmap (H.foldrWithKey (M.insert . unsafeCoerce) M.empty) . H.traverseWithKey (\k v -> p v <?> Key k)
        FromJSONKeyText f -> withObject "Map k v" $
            fmap (H.foldrWithKey (M.insert . f) M.empty) . H.traverseWithKey (\k v -> p v <?> Key k)
        FromJSONKeyTextParser f -> withObject "Map k v" $
            H.foldrWithKey (\k v m -> M.insert <$> f k <*> (p v <?> Key k) <*> m) (pure M.empty)
        FromJSONKeyValue f -> withArray "Map k v" $ \arr ->
            M.fromList <$> (Tr.sequence .
                zipWith (parseIndexedJSONPair f p) [0..] . V.toList $ arr)
    {-# INLINE liftParseJSON #-}

instance (ToJSON v, ToJSONKey k) => ToJSON (M.Map k v) where
    toJSON = toJSON1
    {-# INLINE toJSON #-}

    toEncoding = toEncoding1
    {-# INLINE toEncoding #-}

instance (FromJSON v, FromJSONKey k, Ord k) => FromJSON (M.Map k v) where
    parseJSON = parseJSON1
    {-# INLINE parseJSON #-}


instance ToJSON1 Tree.Tree where
    liftToJSON to tol = go
      where
        go (Tree.Node root branches) =
            liftToJSON2 to tol to' tol' (root, branches)

        to' = liftToJSON go (listValue go)
        tol' = liftToJSONList go (listValue go)
    {-# INLINE liftToJSON #-}

    liftToEncoding to tol = go
      where
        go (Tree.Node root branches) =
            liftToEncoding2 to tol to' tol' (root, branches)

        to' = liftToEncoding go (listEncoding go)
        tol' = liftToEncodingList go (listEncoding go)
    {-# INLINE liftToEncoding #-}

instance (ToJSON v) => ToJSON (Tree.Tree v) where
    toJSON = toJSON1
    {-# INLINE toJSON #-}

    toEncoding = toEncoding1
    {-# INLINE toEncoding #-}

instance FromJSON1 Tree.Tree where
    liftParseJSON p pl = go
      where
        go v = uncurry Tree.Node <$> liftParseJSON2 p pl p' pl' v

        p' = liftParseJSON go (listParser go)
        pl'= liftParseJSONList go (listParser go)

instance (FromJSON v) => FromJSON (Tree.Tree v) where
    parseJSON = parseJSON1
    {-# INLINE parseJSON #-}

-------------------------------------------------------------------------------
-- vector
-------------------------------------------------------------------------------

instance ToJSON1 Vector where
    liftToJSON to _ = Array . V.map to
    {-# INLINE liftToJSON #-}

    liftToEncoding to _ xs
        | V.null xs = emptyArray_
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
  | VG.null xs = emptyArray_
  | otherwise  = Encoding $
                 B.char7 '[' <> builder (VG.unsafeHead xs) <>
                 VG.foldr go (B.char7 ']') (VG.unsafeTail xs)
    where go v b = B.char7 ',' <> builder v <> b
{-# INLINE encodeVector #-}

instance FromJSON1 Vector where
    liftParseJSON p _ = withArray "Vector a" $
        V.mapM (uncurry $ parseIndexedJSON p) . V.indexed
    {-# INLINE liftParseJSON #-}

instance (FromJSON a) => FromJSON (Vector a) where
    parseJSON = parseJSON1
    {-# INLINE parseJSON #-}

vectorToJSON :: (VG.Vector v a, ToJSON a) => v a -> Value
vectorToJSON = Array . V.map toJSON . V.convert
{-# INLINE vectorToJSON #-}

vectorParseJSON :: (FromJSON a, VG.Vector w a) => String -> Value -> Parser (w a)
vectorParseJSON s = withArray s $ fmap V.convert . V.mapM (uncurry $ parseIndexedJSON parseJSON) . V.indexed
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

-------------------------------------------------------------------------------
-- unordered-containers
-------------------------------------------------------------------------------

instance ToJSON1 HashSet.HashSet where
    liftToJSON to _ = listValue to . HashSet.toList
    {-# INLINE liftToJSON #-}

    liftToEncoding to _ = listEncoding to . HashSet.toList
    {-# INLINE liftToEncoding #-}

instance (ToJSON a) => ToJSON (HashSet.HashSet a) where
    toJSON = toJSON1
    {-# INLINE toJSON #-}

    toEncoding = toEncoding1
    {-# INLINE toEncoding #-}

instance (Eq a, Hashable a, FromJSON a) => FromJSON (HashSet.HashSet a) where
    parseJSON = fmap HashSet.fromList . parseJSON
    {-# INLINE parseJSON #-}


instance ToJSONKey k => ToJSON1 (H.HashMap k) where
    liftToJSON g _ = case toJSONKey of
        ToJSONKeyText (f,_) -> Object . mapKeyVal f g
        ToJSONKeyValue (f,_) -> Array . V.fromList . map (toJSONPair f g) . H.toList
    {-# INLINE liftToJSON #-}

    -- liftToEncoding :: forall a. (a -> Encoding) -> ([a] -> Encoding) -> H.HashMap k a -> Encoding
    liftToEncoding g _ = case toJSONKey of
        ToJSONKeyText (_,f) -> dict f g H.foldrWithKey
        ToJSONKeyValue (_,f) -> listEncoding (pairEncoding f) . H.toList
      where
        -- pairEncoding :: (k -> Encoding) -> (k, a) -> Encoding
        pairEncoding f (a, b) = tuple $ f a >*< g b
    {-# INLINE liftToEncoding #-}

instance (ToJSON v, ToJSONKey k) => ToJSON (H.HashMap k v) where
    toJSON = toJSON1
    {-# INLINE toJSON #-}

    toEncoding = toEncoding1
    {-# INLINE toEncoding #-}

instance (FromJSONKey k, Eq k, Hashable k) => FromJSON1 (H.HashMap k) where
    liftParseJSON p _ = case fromJSONKey of
        FromJSONKeyCoerce _ -> withObject "HashMap ~Text v" $
            uc . H.traverseWithKey (\k v -> p v <?> Key k)
        FromJSONKeyText f -> withObject "HashMap k v" $
            fmap (mapKey f) . H.traverseWithKey (\k v -> p v <?> Key k)
        FromJSONKeyTextParser f -> withObject "HashMap k v" $
            H.foldrWithKey (\k v m -> H.insert <$> f k <*> (p v <?> Key k) <*> m) (pure H.empty)
        FromJSONKeyValue f -> withArray "Map k v" $ \arr ->
            H.fromList <$> (Tr.sequence .
                zipWith (parseIndexedJSONPair f p) [0..] . V.toList $ arr)
      where
        uc :: Parser (H.HashMap Text v) -> Parser (H.HashMap k v)
        uc = unsafeCoerce

instance (FromJSON v, FromJSONKey k, Eq k, Hashable k) => FromJSON (H.HashMap k v) where
    parseJSON = parseJSON1
    {-# INLINE parseJSON #-}

-------------------------------------------------------------------------------
-- aeson
-------------------------------------------------------------------------------

instance ToJSON Value where
    toJSON a = a
    {-# INLINE toJSON #-}

    toEncoding = Encoding . EB.encodeToBuilder
    {-# INLINE toEncoding #-}

instance FromJSON Value where
    parseJSON = pure
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

-------------------------------------------------------------------------------
-- time
-------------------------------------------------------------------------------

instance ToJSON Day where
    toJSON       = stringEncoding
    toEncoding z = Encoding (EB.quote $ EB.day z)

instance FromJSON Day where
    parseJSON = withText "Day" (Time.run Time.day)

instance ToJSON TimeOfDay where
    toJSON       = stringEncoding
    toEncoding z = Encoding (EB.quote $ EB.timeOfDay z)

instance FromJSON TimeOfDay where
    parseJSON = withText "TimeOfDay" (Time.run Time.timeOfDay)

instance ToJSON LocalTime where
    toJSON       = stringEncoding
    toEncoding z = Encoding (EB.quote $ EB.localTime z)

instance FromJSON LocalTime where
    parseJSON = withText "LocalTime" (Time.run Time.localTime)

instance ToJSON ZonedTime where
    toJSON = stringEncoding

    toEncoding z = Encoding (EB.quote $ EB.zonedTime z)

formatMillis :: (FormatTime t) => t -> String
formatMillis = take 3 . formatTime defaultTimeLocale "%q"

instance FromJSON ZonedTime where
    parseJSON = withText "ZonedTime" (Time.run Time.zonedTime)

instance ToJSON UTCTime where
    toJSON = stringEncoding

    toEncoding t = Encoding (EB.quote $ EB.utcTime t)

-- | Encode something to a JSON string.
--
-- TODO: remove me, this is a hack
stringEncoding :: (ToJSON a) => a -> Value
stringEncoding = String
    . T.dropAround (== '"')
    . T.decodeLatin1
    . L.toStrict
    . B.toLazyByteString
    . fromEncoding
    . toEncoding 
{-# INLINE stringEncoding #-}

instance FromJSON UTCTime where
    parseJSON = withText "UTCTime" (Time.run Time.utcTime)

instance ToJSON NominalDiffTime where
    toJSON = Number . realToFrac
    {-# INLINE toJSON #-}

    toEncoding = Encoding . EB.number . realToFrac
    {-# INLINE toEncoding #-}

-- | /WARNING:/ Only parse lengths of time from trusted input
-- since an attacker could easily fill up the memory of the target
-- system by specifying a scientific number with a big exponent like
-- @1e1000000000@.
instance FromJSON NominalDiffTime where
    parseJSON = withScientific "NominalDiffTime" $ pure . realToFrac
    {-# INLINE parseJSON #-}

-------------------------------------------------------------------------------
-- base Monoid/Semigroup
-------------------------------------------------------------------------------

instance ToJSON1 Dual where
    liftToJSON to _ = to . getDual
    {-# INLINE liftToJSON #-}

    liftToEncoding to _ = to . getDual
    {-# INLINE liftToEncoding #-}

instance ToJSON a => ToJSON (Dual a) where
    toJSON = toJSON1
    {-# INLINE toJSON #-}

    toEncoding = toEncoding1
    {-# INLINE toEncoding #-}

instance FromJSON1 Dual where
    liftParseJSON p _ = fmap Dual . p
    {-# INLINE liftParseJSON #-}

instance FromJSON a => FromJSON (Dual a) where
    parseJSON = parseJSON1
    {-# INLINE parseJSON #-}


instance ToJSON1 First where
    liftToJSON to to' = liftToJSON to to' . getFirst
    {-# INLINE liftToJSON #-}

    liftToEncoding to to' = liftToEncoding to to' . getFirst
    {-# INLINE liftToEncoding #-}

instance ToJSON a => ToJSON (First a) where
    toJSON = toJSON1
    {-# INLINE toJSON #-}

    toEncoding = toEncoding1
    {-# INLINE toEncoding #-}

instance FromJSON1 First where
    liftParseJSON p p' = fmap First . liftParseJSON p p'
    {-# INLINE liftParseJSON #-}

instance FromJSON a => FromJSON (First a) where
    parseJSON = parseJSON1
    {-# INLINE parseJSON #-}


instance ToJSON1 Last where
    liftToJSON to to' = liftToJSON to to' . getLast
    {-# INLINE liftToJSON #-}

    liftToEncoding to to' = liftToEncoding to to' . getLast
    {-# INLINE liftToEncoding #-}

instance ToJSON a => ToJSON (Last a) where
    toJSON = toJSON1
    {-# INLINE toJSON #-}

    toEncoding = toEncoding1
    {-# INLINE toEncoding #-}

instance FromJSON1 Last where
    liftParseJSON p p' = fmap Last . liftParseJSON p p'
    {-# INLINE liftParseJSON #-}

instance FromJSON a => FromJSON (Last a) where
    parseJSON = parseJSON1
    {-# INLINE parseJSON #-}

-------------------------------------------------------------------------------
-- tagged
-------------------------------------------------------------------------------

instance ToJSON (Proxy a) where
    toJSON _ = Null
    {-# INLINE toJSON #-}

    toEncoding _ = Encoding EB.null_
    {-# INLINE toEncoding #-}

instance FromJSON (Proxy a) where
    {-# INLINE parseJSON #-}
    parseJSON Null = pure Proxy
    parseJSON v    = typeMismatch "Proxy" v


instance ToJSON1 (Tagged a) where
    liftToJSON to _ (Tagged x) = to x
    {-# INLINE liftToJSON #-}

    liftToEncoding to _ (Tagged x) = to x
    {-# INLINE liftToEncoding #-}

instance ToJSON b => ToJSON (Tagged a b) where
    toJSON = toJSON1
    {-# INLINE toJSON #-}

    toEncoding = toEncoding1
    {-# INLINE toEncoding #-}

instance FromJSON1 (Tagged a) where
    liftParseJSON p _ = fmap Tagged . p
    {-# INLINE liftParseJSON #-}

instance FromJSON b => FromJSON (Tagged a b) where
    parseJSON = parseJSON1
    {-# INLINE parseJSON #-}

instance FromJSONKey b => FromJSONKey (Tagged a b) where
    fromJSONKey = coerceFromJSONKeyFunction (fromJSONKey :: FromJSONKeyFunction b)
    fromJSONKeyList = (fmap . fmap) Tagged fromJSONKeyList

instance ToJSONKey b => ToJSONKey (Tagged a b) where
    toJSONKey = contramapToJSONKeyFunction unTagged toJSONKey
    toJSONKeyList = contramapToJSONKeyFunction (fmap unTagged) toJSONKeyList

-------------------------------------------------------------------------------
-- Instances for converting to/from map keys
-------------------------------------------------------------------------------



instance ToJSONKey Text where
    toJSONKey = ToJSONKeyText (id,toEncoding)

instance FromJSONKey Text where
    fromJSONKey = fromJSONKeyCoerce



instance ToJSONKey Bool where
    toJSONKey = ToJSONKeyText
        ( (\x -> if x then "true" else "false")
        , (\x -> Encoding $ if x then "\"true\"" else "\"false\"")
        )

instance ToJSONKey Int where
    toJSONKey = ToJSONKeyText
        ( LT.toStrict . LTB.toLazyText . LTBI.decimal
        , \x -> Encoding $ B.char7 '"' <> fromEncoding (toEncoding x) <> B.char7 '"'
        )

instance FromJSONKey Int where
    -- not sure if there if there is already a helper in
    -- aeson for doing this.
    fromJSONKey = FromJSONKeyTextParser $ \t -> case TR.decimal t of
      Left err -> fail err
      Right (v,t2) -> if T.null t2
        then return v
        else fail "Was not an integer, had extra stuff."

instance (ToJSON a, ToJSON b) => ToJSONKey (a,b)
instance (ToJSON a, ToJSON b, ToJSON c) => ToJSONKey (a,b,c)
instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d) => ToJSONKey (a,b,c,d)

instance (FromJSON a, FromJSON b) => FromJSONKey (a,b)
instance (FromJSON a, FromJSON b, FromJSON c) => FromJSONKey (a,b,c)
instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d) => FromJSONKey (a,b,c,d)

instance ToJSONKey Char where
    toJSONKey = ToJSONKeyText (T.singleton, toEncoding)
    toJSONKeyList = ToJSONKeyText (T.pack, toEncoding . T.pack)

instance FromJSONKey Char where
    fromJSONKey = FromJSONKeyTextParser $ \t ->
        if T.length t == 1
            then return (T.index t 0)
            else typeMismatch "Expected Char but String didn't contain exactly one character" (String t)
    fromJSONKeyList = FromJSONKeyText T.unpack

instance (ToJSONKey a, ToJSON a) => ToJSONKey [a] where
    toJSONKey = toJSONKeyList

instance (FromJSONKey a, FromJSON a) => FromJSONKey [a] where
    fromJSONKey = fromJSONKeyList

instance (ToJSONKey a, ToJSON a) => ToJSONKey (Identity a) where
    toJSONKey = contramapToJSONKeyFunction runIdentity toJSONKey

instance (FromJSONKey a, FromJSON a) => FromJSONKey (Identity a) where
    fromJSONKey = mapFromJSONKeyFunction Identity fromJSONKey

-------------------------------------------------------------------------------
-- Functions
-------------------------------------------------------------------------------

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
               Just v  -> parseJSON v <?> Key key
{-# INLINE (.:) #-}

-- | Retrieve the value associated with the given key of an 'Object'.
-- The result is 'Nothing' if the key is not present, or 'empty' if
-- the value cannot be converted to the desired type.
--
-- This accessor is most useful if the key and value can be absent
-- from an object without affecting its validity.  If the key and
-- value are mandatory, use '.:' instead.
(.:?) :: (FromJSON a) => Object -> Text -> Parser (Maybe a)
obj .:? key = parseOptionalFieldWith parseJSON obj key
{-# INLINE (.:?) #-}

parseOptionalFieldWith :: (Value -> Parser (Maybe a))
                       -> Object -> Text -> Parser (Maybe a)
parseOptionalFieldWith pj obj key =
    case H.lookup key obj of
     Nothing -> pure Nothing
     Just v  -> pj v <?> Key key
{-# INLINE parseOptionalFieldWith #-}

-- | Like '.:?', but the resulting parser will fail,
-- if the key is present but is 'Null'.
(.:!) :: (FromJSON a) => Object -> Text -> Parser (Maybe a)
obj .:! key = case H.lookup key obj of
               Nothing -> pure Nothing
               Just v  -> Just <$> parseJSON v <?> Key key
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
