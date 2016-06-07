{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternGuards         #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}

#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE PolyKinds             #-}
#endif

#include "overlapping-compat.h"

-- TODO: Drop this when we remove support for Data.Attoparsec.Number
{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Data.Aeson.Types.ToJSON (
    -- * Core JSON classes
      ToJSON(..)
    -- * Liftings to unary and binary type constructors
    , ToJSON1(..)
    , toJSON1
    , toEncoding1
    , ToJSON2(..)
    , toJSON2
    , toEncoding2
    -- * Generic JSON classes
    , GToJSON(..)
    , GToEncoding(..)
    , genericToJSON
    , genericToEncoding
    -- * Classes and types for map keys
    , ToJSONKey(..)
    , ToJSONKeyFunction(..)
    , contramapToJSONKeyFunction
    -- * Object key-value pairs
    , KeyValue(..)
    -- * Functions needed for documentation
    -- * Encoding functions
    , listEncoding
    , listValue
    ) where

import Prelude        ()
import Prelude.Compat

import Data.Aeson.Encoding.Internal (Encoding (..), Series, dict, emptyArray_,
                                     tuple, (>*<))
import Data.Aeson.Functions         (mapHashKeyVal, mapKeyVal)
import Data.Aeson.Types.Generic
import Data.Aeson.Types.Internal

import qualified Data.Aeson.Encode.Builder    as EB
import qualified Data.Aeson.Encoding.Internal as E

import Control.Applicative     (Const (..))
import Control.Monad.ST        (ST)
import Data.Attoparsec.Number  (Number (..))
import Data.Bits               (unsafeShiftR)
import Data.DList              (DList)
import Data.Fixed              (Fixed, HasResolution)
import Data.Foldable           (toList)
import Data.Functor.Identity   (Identity (..))
import Data.Int                (Int16, Int32, Int64, Int8)
import Data.List               (intersperse)
import Data.List.NonEmpty      (NonEmpty (..))
import Data.Monoid             (Dual (..), First (..), Last (..), (<>))
import Data.Proxy              (Proxy (..))
import Data.Ratio              (Ratio, denominator, numerator)
import Data.Scientific         (Scientific)
import Data.Tagged             (Tagged (..))
import Data.Text               (Text, pack)
import Data.Time               (Day, LocalTime, NominalDiffTime, TimeOfDay,
                                UTCTime, ZonedTime)
import Data.Time.Format        (FormatTime, formatTime)
import Data.Time.Locale.Compat (defaultTimeLocale)
import Data.Vector             (Vector)
import Data.Version            (Version, showVersion)
import Data.Word               (Word16, Word32, Word64, Word8)
import Foreign.Storable        (Storable)
import GHC.Generics
import Numeric.Natural         (Natural)

import qualified Data.ByteString.Builder    as B
import qualified Data.ByteString.Lazy       as L
import qualified Data.DList                 as DList
import qualified Data.HashMap.Strict        as H
import qualified Data.HashSet               as HashSet
import qualified Data.IntMap                as IntMap
import qualified Data.IntSet                as IntSet
import qualified Data.List.NonEmpty         as NE
import qualified Data.Map                   as M
import qualified Data.Scientific            as Scientific
import qualified Data.Sequence              as Seq
import qualified Data.Set                   as Set
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import qualified Data.Text.Lazy             as LT
import qualified Data.Text.Lazy.Builder     as LTB
import qualified Data.Text.Lazy.Builder.Int as LTBI
import qualified Data.Tree                  as Tree
import qualified Data.Vector                as V
import qualified Data.Vector.Generic        as VG
import qualified Data.Vector.Mutable        as VM
import qualified Data.Vector.Primitive      as VP
import qualified Data.Vector.Storable       as VS
import qualified Data.Vector.Unboxed        as VU

builder :: ToJSON a => a -> B.Builder
builder = fromEncoding . toEncoding

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
-------------------------------------------------------------------------------
-- Generics
-------------------------------------------------------------------------------

-- | Class of generic representation types ('Rep') that can be converted to
-- JSON.
class GToJSON f where
    -- | This method (applied to 'defaultOptions') is used as the
    -- default generic implementation of 'toJSON'.
    gToJSON :: Options -> f a -> Value

-- | Class of generic representation types ('Rep') that can be converted to
-- a JSON 'Encoding'.
class GToEncoding f where
    -- | This method (applied to 'defaultOptions') can be used as the
    -- default generic implementation of 'toEncoding'.
    gToEncoding :: Options -> f a -> Encoding


-- | A configurable generic JSON creator. This function applied to
-- 'defaultOptions' is used as the default for 'toJSON' when the type
-- is an instance of 'Generic'.
genericToJSON :: (Generic a, GToJSON (Rep a)) => Options -> a -> Value
genericToJSON opts = gToJSON opts . from

-- | A configurable generic JSON encoder. This function applied to
-- 'defaultOptions' is used as the default for 'toEncoding' when the type
-- is an instance of 'Generic'.
genericToEncoding :: (Generic a, GToEncoding (Rep a)) => Options -> a -> Encoding
genericToEncoding opts = gToEncoding opts . from

-------------------------------------------------------------------------------
-- Class
-------------------------------------------------------------------------------

-- | A type that can be converted to JSON.
--
-- An example type and instance:
--
-- @
-- \-- Allow ourselves to write 'Text' literals.
-- {-\# LANGUAGE OverloadedStrings #-}
--
-- data Coord = Coord { x :: Double, y :: Double }
--
-- instance ToJSON Coord where
--   toJSON (Coord x y) = 'object' [\"x\" '.=' x, \"y\" '.=' y]
--
--   toEncoding (Coord x y) = 'pairs' (\"x\" '.=' x '<>' \"y\" '.=' y)
-- @
--
-- Instead of manually writing your 'ToJSON' instance, there are two options
-- to do it automatically:
--
-- * "Data.Aeson.TH" provides Template Haskell functions which will derive an
-- instance at compile time. The generated instance is optimized for your type
-- so will probably be more efficient than the following two options:
--
-- * The compiler can provide a default generic implementation for
-- 'toJSON'.
--
-- To use the second, simply add a @deriving 'Generic'@ clause to your
-- datatype and declare a 'ToJSON' instance for your datatype without giving
-- definitions for 'toJSON' or 'toEncoding'.
--
-- For example, the previous example can be simplified to a more
-- minimal instance:
--
-- @
-- {-\# LANGUAGE DeriveGeneric \#-}
--
-- import "GHC.Generics"
--
-- data Coord = Coord { x :: Double, y :: Double } deriving 'Generic'
--
-- instance ToJSON Coord where
--     toEncoding = 'genericToEncoding' 'defaultOptions'
-- @
--
-- Why do we provide an implementation for 'toEncoding' here?  The
-- 'toEncoding' function is a relatively new addition to this class.
-- To allow users of older versions of this library to upgrade without
-- having to edit all of their instances or encounter surprising
-- incompatibilities, the default implementation of 'toEncoding' uses
-- 'toJSON'.  This produces correct results, but since it performs an
-- intermediate conversion to a 'Value', it will be less efficient
-- than directly emitting an 'Encoding'.  Our one-liner definition of
-- 'toEncoding' above bypasses the intermediate 'Value'.
--
-- If @DefaultSignatures@ doesn't give exactly the results you want,
-- you can customize the generic encoding with only a tiny amount of
-- effort, using 'genericToJSON' and 'genericToEncoding' with your
-- preferred 'Options':
--
-- @
-- instance ToJSON Coord where
--     toJSON     = 'genericToJSON' 'defaultOptions'
--     toEncoding = 'genericToEncoding' 'defaultOptions'
-- @
class ToJSON a where
    -- | Convert a Haskell value to a JSON-friendly intermediate type.
    toJSON     :: a -> Value

    default toJSON :: (Generic a, GToJSON (Rep a)) => a -> Value
    toJSON = genericToJSON defaultOptions

    -- | Encode a Haskell value as JSON.
    --
    -- The default implementation of this method creates an
    -- intermediate 'Value' using 'toJSON'.  This provides
    -- source-level compatibility for people upgrading from older
    -- versions of this library, but obviously offers no performance
    -- advantage.
    --
    -- To benefit from direct encoding, you /must/ provide an
    -- implementation for this method.  The easiest way to do so is by
    -- having your types implement 'Generic' using the @DeriveGeneric@
    -- extension, and then have GHC generate a method body as follows.
    --
    -- @
    -- instance ToJSON Coord where
    --     toEncoding = 'genericToEncoding' 'defaultOptions'
    -- @

    toEncoding :: a -> Encoding
    toEncoding = E.Encoding . EB.encodeToBuilder . toJSON
    {-# INLINE toEncoding #-}

    toJSONList :: [a] -> Value
    toJSONList = listValue toJSON
    {-# INLINE toJSONList #-}

    toEncodingList :: [a] -> Encoding
    toEncodingList = listEncoding toEncoding
    {-# INLINE toEncodingList #-}

-------------------------------------------------------------------------------
-- Object key-value pairs
-------------------------------------------------------------------------------

-- | A key-value pair for encoding a JSON object.
class KeyValue kv where
    (.=) :: ToJSON v => Text -> v -> kv
    infixr 8 .=

instance KeyValue Series where
    name .= value = E.Value $ E.text name <> E.colon <> toEncoding value
    {-# INLINE (.=) #-}

instance KeyValue Pair where
    name .= value = (name, toJSON value)
    {-# INLINE (.=) #-}

-------------------------------------------------------------------------------
--  Classes and types for map keys
-------------------------------------------------------------------------------

class ToJSONKey a where
    toJSONKey :: ToJSONKeyFunction a
    default toJSONKey :: ToJSON a => ToJSONKeyFunction a
    toJSONKey = ToJSONKeyValue (toJSON, toEncoding)
    toJSONKeyList :: ToJSONKeyFunction [a]
    default toJSONKeyList :: ToJSON a => ToJSONKeyFunction [a]
    toJSONKeyList = ToJSONKeyValue (toJSON, toEncoding)

data ToJSONKeyFunction a
    = ToJSONKeyText (a -> Text, a -> Encoding)
    | ToJSONKeyValue (a -> Value, a -> Encoding)

contramapToJSONKeyFunction :: (b -> a) -> ToJSONKeyFunction a -> ToJSONKeyFunction b
contramapToJSONKeyFunction h x = case x of
    ToJSONKeyText (f,g) -> ToJSONKeyText (f . h, g . h)
    ToJSONKeyValue (f,g) -> ToJSONKeyValue (f . h, g . h)

-------------------------------------------------------------------------------
-- Lifings of FromJSON and ToJSON to unary and binary type constructors
-------------------------------------------------------------------------------

-- | Lifting of the 'ToJSON' class to unary type constructors.
class ToJSON1 f where
    liftToJSON :: (a -> Value) -> ([a] -> Value) -> f a -> Value
    liftToJSONList :: (a -> Value) -> ([a] -> Value) -> [f a] -> Value
    liftToJSONList f g = listValue (liftToJSON f g)

    -- | Unfortunately there cannot be a default implementation of 'liftToEncoding'.
    liftToEncoding :: (a -> Encoding) -> ([a] -> Encoding) -> f a -> Encoding
    liftToEncodingList :: (a -> Encoding) -> ([a] -> Encoding) -> [f a] -> Encoding
    liftToEncodingList f g = listEncoding (liftToEncoding f g)

-- | Lift the standard 'toJSON' function through the type constructor.
toJSON1 :: (ToJSON1 f, ToJSON a) => f a -> Value
toJSON1 = liftToJSON toJSON toJSONList
{-# INLINE toJSON1 #-}

-- | Lift the standard 'toEncoding' function through the type constructor.
toEncoding1 :: (ToJSON1 f, ToJSON a) => f a -> Encoding
toEncoding1 = liftToEncoding toEncoding toEncodingList
{-# INLINE toEncoding1 #-}


-- | Lifting of the 'ToJSON' class to binary type constructors.
class ToJSON2 f where
    liftToJSON2 :: (a -> Value) -> ([a] -> Value) -> (b -> Value) -> ([b] -> Value) -> f a b -> Value
    liftToJSONList2 :: (a -> Value) -> ([a] -> Value) -> (b -> Value) -> ([b] -> Value) -> [f a b] -> Value
    liftToJSONList2 fa ga fb gb = listValue (liftToJSON2 fa ga fb gb)

    liftToEncoding2 :: (a -> Encoding) -> ([a] -> Encoding) -> (b -> Encoding) -> ([b] -> Encoding) -> f a b -> Encoding
    liftToEncodingList2 :: (a -> Encoding) -> ([a] -> Encoding) -> (b -> Encoding) -> ([b] -> Encoding) -> [f a b] -> Encoding
    liftToEncodingList2 fa ga fb gb = listEncoding (liftToEncoding2 fa ga fb gb)

-- | Lift the standard 'toJSON' function through the type constructor.
toJSON2 :: (ToJSON2 f, ToJSON a, ToJSON b) => f a b -> Value
toJSON2 = liftToJSON2 toJSON toJSONList toJSON toJSONList
{-# INLINE toJSON2 #-}

-- | Lift the standard 'toEncoding' function through the type constructor.
toEncoding2 :: (ToJSON2 f, ToJSON a, ToJSON b) => f a b -> Encoding
toEncoding2 = liftToEncoding2 toEncoding toEncodingList toEncoding toEncodingList
{-# INLINE toEncoding2 #-}

-------------------------------------------------------------------------------
-- Encoding functions
-------------------------------------------------------------------------------

listEncoding :: (a -> Encoding) -> [a] -> Encoding
listEncoding = E.list

listValue :: (a -> Value) -> [a] -> Value
listValue f = Array . V.fromList . map f
{-# INLINE listValue #-}

-------------------------------------------------------------------------------
-- [] instances
-------------------------------------------------------------------------------

-- These are needed for key-class default definitions

instance ToJSON1 [] where
    liftToJSON _ to' = to'
    {-# INLINE liftToJSON #-}

    liftToEncoding _ to' = to'
    {-# INLINE liftToEncoding #-}

instance (ToJSON a) => ToJSON [a] where
    toJSON = toJSON1
    {-# INLINE toJSON #-}

    toEncoding = toEncoding1
    {-# INLINE toEncoding #-}

-------------------------------------------------------------------------------
-- Generic toJSON / toEncoding
-------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Generic toJSON

instance OVERLAPPABLE_ (GToJSON a) => GToJSON (M1 i c a) where
    -- Meta-information, which is not handled elsewhere, is ignored:
    gToJSON opts = gToJSON opts . unM1

instance (ToJSON a) => GToJSON (K1 i a) where
    -- Constant values are encoded using their ToJSON instance:
    gToJSON _opts = toJSON . unK1

instance GToJSON U1 where
    -- Empty constructors are encoded to an empty array:
    gToJSON _opts _ = emptyArray

instance (ConsToJSON a) => GToJSON (C1 c a) where
    -- Constructors need to be encoded differently depending on whether they're
    -- a record or not. This distinction is made by 'consToJSON':
    gToJSON opts = consToJSON opts . unM1

instance ( WriteProduct a, WriteProduct b
         , ProductSize  a, ProductSize  b ) => GToJSON (a :*: b) where
    -- Products are encoded to an array. Here we allocate a mutable vector of
    -- the same size as the product and write the product's elements to it using
    -- 'writeProduct':
    gToJSON opts p =
        Array $ V.create $ do
          mv <- VM.unsafeNew lenProduct
          writeProduct opts mv 0 lenProduct p
          return mv
        where
          lenProduct = (unTagged2 :: Tagged2 (a :*: b) Int -> Int)
                       productSize

instance ( AllNullary (a :+: b) allNullary
         , SumToJSON  (a :+: b) allNullary ) => GToJSON (a :+: b) where
    -- If all constructors of a sum datatype are nullary and the
    -- 'allNullaryToStringTag' option is set they are encoded to
    -- strings.  This distinction is made by 'sumToJSON':
    gToJSON opts = (unTagged :: Tagged allNullary Value -> Value)
                 . sumToJSON opts

--------------------------------------------------------------------------------
-- Generic toEncoding

instance OVERLAPPABLE_ (GToEncoding a) => GToEncoding (M1 i c a) where
    -- Meta-information, which is not handled elsewhere, is ignored:
    gToEncoding opts = gToEncoding opts . unM1

instance (ToJSON a) => GToEncoding (K1 i a) where
    -- Constant values are encoded using their ToJSON instance:
    gToEncoding _opts = toEncoding . unK1

instance GToEncoding U1 where
    -- Empty constructors are encoded to an empty array:
    gToEncoding _opts _ = E.emptyArray_

instance (ConsToEncoding a) => GToEncoding (C1 c a) where
    -- Constructors need to be encoded differently depending on whether they're
    -- a record or not. This distinction is made by 'consToEncoding':
    gToEncoding opts = consToEncoding opts . unM1

instance ( EncodeProduct a, EncodeProduct b ) => GToEncoding (a :*: b) where
    -- Products are encoded to an array. Here we allocate a mutable vector of
    -- the same size as the product and write the product's elements to it using
    -- 'encodeProduct':
    gToEncoding opts p = E.tuple $ encodeProduct opts p

instance ( AllNullary    (a :+: b) allNullary
         , SumToEncoding (a :+: b) allNullary ) => GToEncoding (a :+: b) where
    -- If all constructors of a sum datatype are nullary and the
    -- 'allNullaryToStringTag' option is set they are encoded to
    -- strings.  This distinction is made by 'sumToEncoding':
    gToEncoding opts
        = (unTagged :: Tagged allNullary Encoding -> Encoding)
        . sumToEncoding opts

--------------------------------------------------------------------------------

class SumToJSON f allNullary where
    sumToJSON :: Options -> f a -> Tagged allNullary Value

instance ( GetConName               f
         , TaggedObjectPairs        f
         , ObjectWithSingleFieldObj f
         , TwoElemArrayObj          f ) => SumToJSON f True where
    sumToJSON opts
        | allNullaryToStringTag opts = Tagged . String . pack
                                     . constructorTagModifier opts . getConName
        | otherwise = Tagged . nonAllNullarySumToJSON opts

instance ( TwoElemArrayObj          f
         , TaggedObjectPairs        f
         , ObjectWithSingleFieldObj f ) => SumToJSON f False where
    sumToJSON opts = Tagged . nonAllNullarySumToJSON opts

nonAllNullarySumToJSON :: ( TwoElemArrayObj          f
                          , TaggedObjectPairs        f
                          , ObjectWithSingleFieldObj f
                          ) => Options -> f a -> Value
nonAllNullarySumToJSON opts =
    case sumEncoding opts of
      TaggedObject{..}      ->
        object . taggedObjectPairs opts tagFieldName contentsFieldName
      ObjectWithSingleField -> Object . objectWithSingleFieldObj opts
      TwoElemArray          -> Array  . twoElemArrayObj opts

--------------------------------------------------------------------------------

class SumToEncoding f allNullary where
    sumToEncoding :: Options -> f a -> Tagged allNullary Encoding

instance ( GetConName               f
         , TaggedObjectEnc          f
         , ObjectWithSingleFieldEnc f
         , TwoElemArrayEnc          f ) => SumToEncoding f True where
    sumToEncoding opts
        | allNullaryToStringTag opts = Tagged . toEncoding .
                                       constructorTagModifier opts . getConName
        | otherwise = Tagged . nonAllNullarySumToEncoding opts

instance ( TwoElemArrayEnc          f
         , TaggedObjectEnc          f
         , ObjectWithSingleFieldEnc f ) => SumToEncoding f False where
    sumToEncoding opts = Tagged . nonAllNullarySumToEncoding opts

nonAllNullarySumToEncoding :: ( TwoElemArrayEnc          f
                              , TaggedObjectEnc          f
                              , ObjectWithSingleFieldEnc f
                              ) => Options -> f a -> Encoding
nonAllNullarySumToEncoding opts =
    case sumEncoding opts of
      TaggedObject{..}      ->
        taggedObjectEnc opts tagFieldName contentsFieldName
      ObjectWithSingleField -> objectWithSingleFieldEnc opts
      TwoElemArray          -> twoElemArrayEnc opts

--------------------------------------------------------------------------------

class TaggedObjectPairs f where
    taggedObjectPairs :: Options -> String -> String -> f a -> [Pair]

instance ( TaggedObjectPairs a
         , TaggedObjectPairs b ) => TaggedObjectPairs (a :+: b) where
    taggedObjectPairs opts tagFieldName contentsFieldName (L1 x) =
        taggedObjectPairs opts tagFieldName contentsFieldName     x
    taggedObjectPairs opts tagFieldName contentsFieldName (R1 x) =
        taggedObjectPairs opts tagFieldName contentsFieldName     x

instance ( IsRecord           a isRecord
         , TaggedObjectPairs' a isRecord
         , Constructor c ) => TaggedObjectPairs (C1 c a) where
    taggedObjectPairs opts tagFieldName contentsFieldName =
        (pack tagFieldName .= constructorTagModifier opts
                                 (conName (undefined :: t c a p)) :) .
        (unTagged :: Tagged isRecord [Pair] -> [Pair]) .
          taggedObjectPairs' opts contentsFieldName . unM1

class TaggedObjectPairs' f isRecord where
    taggedObjectPairs' :: Options -> String -> f a -> Tagged isRecord [Pair]

instance OVERLAPPING_ TaggedObjectPairs' U1 False where
    taggedObjectPairs' _ _ _ = Tagged []

instance (RecordToPairs f) => TaggedObjectPairs' f True where
    taggedObjectPairs' opts _ = Tagged . toList . recordToPairs opts

instance (GToJSON f) => TaggedObjectPairs' f False where
    taggedObjectPairs' opts contentsFieldName =
        Tagged . (:[]) . (pack contentsFieldName .=) . gToJSON opts

--------------------------------------------------------------------------------

class TaggedObjectEnc f where
    taggedObjectEnc :: Options -> String -> String -> f a -> Encoding

instance ( TaggedObjectEnc a
         , TaggedObjectEnc b ) => TaggedObjectEnc (a :+: b) where
    taggedObjectEnc opts tagFieldName contentsFieldName (L1 x) =
        taggedObjectEnc opts tagFieldName contentsFieldName     x
    taggedObjectEnc opts tagFieldName contentsFieldName (R1 x) =
        taggedObjectEnc opts tagFieldName contentsFieldName     x

instance ( IsRecord         a isRecord
         , TaggedObjectEnc' a isRecord
         , Constructor c ) => TaggedObjectEnc (C1 c a) where
    taggedObjectEnc opts tagFieldName contentsFieldName v =
        E.openCurly <>
        (toEncoding tagFieldName <>
         E.colon <>
         toEncoding (constructorTagModifier opts (conName (undefined :: t c a p)))) <>
        ((unTagged :: Tagged isRecord Encoding -> Encoding) .
         taggedObjectEnc' opts contentsFieldName . unM1 $ v) <>
        E.closeCurly

class TaggedObjectEnc' f isRecord where
    taggedObjectEnc' :: Options -> String -> f a -> Tagged isRecord Encoding

instance OVERLAPPING_ TaggedObjectEnc' U1 False where
    taggedObjectEnc' _ _ _ = Tagged mempty

instance (RecordToEncoding f) => TaggedObjectEnc' f True where
    taggedObjectEnc' opts _ = Tagged . (E.comma <>) . fst . recordToEncoding opts

instance (GToEncoding f) => TaggedObjectEnc' f False where
    taggedObjectEnc' opts contentsFieldName =
        Tagged . (\z -> E.comma <> toEncoding contentsFieldName <> E.colon <> z) .
        gToEncoding opts

--------------------------------------------------------------------------------

-- | Get the name of the constructor of a sum datatype.
class GetConName f where
    getConName :: f a -> String

instance (GetConName a, GetConName b) => GetConName (a :+: b) where
    getConName (L1 x) = getConName x
    getConName (R1 x) = getConName x

instance (Constructor c) => GetConName (C1 c a) where
    getConName = conName

--------------------------------------------------------------------------------

class TwoElemArrayObj f where
    twoElemArrayObj :: Options -> f a -> V.Vector Value

instance (TwoElemArrayObj a, TwoElemArrayObj b) => TwoElemArrayObj (a :+: b) where
    twoElemArrayObj opts (L1 x) = twoElemArrayObj opts x
    twoElemArrayObj opts (R1 x) = twoElemArrayObj opts x

instance ( GToJSON a, ConsToJSON a
         , Constructor c ) => TwoElemArrayObj (C1 c a) where
    twoElemArrayObj opts x = V.create $ do
      mv <- VM.unsafeNew 2
      VM.unsafeWrite mv 0 $ String $ pack $ constructorTagModifier opts
                                   $ conName (undefined :: t c a p)
      VM.unsafeWrite mv 1 $ gToJSON opts x
      return mv

--------------------------------------------------------------------------------

class TwoElemArrayEnc f where
    twoElemArrayEnc :: Options -> f a -> Encoding

instance (TwoElemArrayEnc a, TwoElemArrayEnc b) => TwoElemArrayEnc (a :+: b) where
    twoElemArrayEnc opts (L1 x) = twoElemArrayEnc opts x
    twoElemArrayEnc opts (R1 x) = twoElemArrayEnc opts x

instance ( GToEncoding a, ConsToEncoding a
         , Constructor c ) => TwoElemArrayEnc (C1 c a) where
    twoElemArrayEnc opts x = E.tuple $
      toEncoding (constructorTagModifier opts (conName (undefined :: t c a p))) >*<
      gToEncoding opts x

--------------------------------------------------------------------------------

class ConsToJSON f where
    consToJSON     :: Options -> f a -> Value

class ConsToJSON' f isRecord where
    consToJSON'     :: Options -> Bool -- ^ Are we a record with one field?
                    -> f a -> Tagged isRecord Value

instance ( IsRecord    f isRecord
         , ConsToJSON' f isRecord ) => ConsToJSON f where
    consToJSON opts = (unTagged :: Tagged isRecord Value -> Value)
                    . consToJSON' opts (isUnary (undefined :: f a))

instance (RecordToPairs f) => ConsToJSON' f True where
    consToJSON' opts isUn f = let
      vals = toList $ recordToPairs opts f
      in case (unwrapUnaryRecords opts,isUn,vals) of
        (True,True,[(_,val)]) -> Tagged val
        _ -> Tagged $ object vals

instance GToJSON f => ConsToJSON' f False where
    consToJSON' opts _ = Tagged . gToJSON opts

--------------------------------------------------------------------------------

class ConsToEncoding f where
    consToEncoding :: Options -> f a -> Encoding

class ConsToEncoding' f isRecord where
    consToEncoding' :: Options -> Bool -- ^ Are we a record with one field?
                    -> f a -> Tagged isRecord Encoding

instance ( IsRecord        f isRecord
         , ConsToEncoding' f isRecord ) => ConsToEncoding f where
    consToEncoding opts = (unTagged :: Tagged isRecord Encoding -> Encoding)
                          . consToEncoding' opts (isUnary (undefined :: f a))

instance (RecordToEncoding f) => ConsToEncoding' f True where
    consToEncoding' opts isUn x =
      let (enc, mbVal) = recordToEncoding opts x
      in case (unwrapUnaryRecords opts, isUn, mbVal) of
           (True, True, Just val) -> Tagged val
           _ -> Tagged $ E.wrapObject enc

instance GToEncoding f => ConsToEncoding' f False where
    consToEncoding' opts _ = Tagged . gToEncoding opts

--------------------------------------------------------------------------------

class RecordToPairs f where
    recordToPairs    :: Options -> f a -> DList Pair

instance (RecordToPairs a, RecordToPairs b) => RecordToPairs (a :*: b) where
    recordToPairs opts (a :*: b) = recordToPairs opts a <>
                                   recordToPairs opts b

instance (Selector s, GToJSON a) => RecordToPairs (S1 s a) where
    recordToPairs = fieldToPair

instance OVERLAPPING_ (Selector s, ToJSON a) =>
  RecordToPairs (S1 s (K1 i (Maybe a))) where
    recordToPairs opts (M1 k1) | omitNothingFields opts
                               , K1 Nothing <- k1 = DList.empty
    recordToPairs opts m1 = fieldToPair opts m1

fieldToPair :: (Selector s, GToJSON a) => Options -> S1 s a p -> DList Pair
fieldToPair opts m1 = pure ( pack $ fieldLabelModifier opts $ selName m1
                           , gToJSON opts (unM1 m1)
                           )

--------------------------------------------------------------------------------

class RecordToEncoding f where
    -- 1st element: whole thing
    -- 2nd element: in case the record has only 1 field, just the value
    --              of the field (without the key); 'Nothing' otherwise
    recordToEncoding :: Options -> f a -> (Encoding, Maybe Encoding)

instance (RecordToEncoding a, RecordToEncoding b) => RecordToEncoding (a :*: b) where
    recordToEncoding opts (a :*: b) | omitNothingFields opts =
      (mconcat $ intersperse E.comma $
        filter (not . E.nullEncoding)
        [fst (recordToEncoding opts a), fst (recordToEncoding opts b)]
      , Nothing)
    recordToEncoding opts (a :*: b) =
      (fst (recordToEncoding opts a) <> E.comma <>
       fst (recordToEncoding opts b),
       Nothing)

instance (Selector s, GToEncoding a) => RecordToEncoding (S1 s a) where
    recordToEncoding = fieldToEncoding

instance OVERLAPPING_ (Selector s, ToJSON a) =>
  RecordToEncoding (S1 s (K1 i (Maybe a))) where
    recordToEncoding opts (M1 k1) | omitNothingFields opts
                                  , K1 Nothing <- k1 = (mempty, Nothing)
    recordToEncoding opts m1 = fieldToEncoding opts m1

fieldToEncoding :: (Selector s, GToEncoding a) => Options -> S1 s a p -> (Encoding, Maybe Encoding)
fieldToEncoding opts m1 =
  let keyBuilder = toEncoding (fieldLabelModifier opts $ selName m1)
      valueBuilder = gToEncoding opts (unM1 m1)
  in  (keyBuilder <> E.colon <> valueBuilder, Just valueBuilder)

--------------------------------------------------------------------------------

class WriteProduct f where
    writeProduct :: Options
                 -> VM.MVector s Value
                 -> Int -- ^ index
                 -> Int -- ^ length
                 -> f a
                 -> ST s ()

instance ( WriteProduct a
         , WriteProduct b ) => WriteProduct (a :*: b) where
    writeProduct opts mv ix len (a :*: b) = do
      writeProduct opts mv ix  lenL a
      writeProduct opts mv ixR lenR b
        where
          lenL = len `unsafeShiftR` 1
          lenR = len - lenL
          ixR  = ix  + lenL

instance OVERLAPPABLE_ (GToJSON a) => WriteProduct a where
    writeProduct opts mv ix _ = VM.unsafeWrite mv ix . gToJSON opts

--------------------------------------------------------------------------------

class EncodeProduct f where
    encodeProduct :: Options -> f a -> Encoding

instance ( EncodeProduct a
         , EncodeProduct b ) => EncodeProduct (a :*: b) where
    encodeProduct opts (a :*: b) | omitNothingFields opts =
        mconcat $ intersperse E.comma $
        filter (not . E.nullEncoding)
        [encodeProduct opts a, encodeProduct opts b]
    encodeProduct opts (a :*: b) = encodeProduct opts a <>
                                   E.comma <>
                                   encodeProduct opts b

instance OVERLAPPABLE_ (GToEncoding a) => EncodeProduct a where
    encodeProduct opts = gToEncoding opts

--------------------------------------------------------------------------------

class ObjectWithSingleFieldObj f where
    objectWithSingleFieldObj :: Options -> f a -> Object

instance ( ObjectWithSingleFieldObj a
         , ObjectWithSingleFieldObj b ) => ObjectWithSingleFieldObj (a :+: b) where
    objectWithSingleFieldObj opts (L1 x) = objectWithSingleFieldObj opts x
    objectWithSingleFieldObj opts (R1 x) = objectWithSingleFieldObj opts x

instance ( GToJSON a, ConsToJSON a
         , Constructor c ) => ObjectWithSingleFieldObj (C1 c a) where
    objectWithSingleFieldObj opts = H.singleton typ . gToJSON opts
        where
          typ = pack $ constructorTagModifier opts $
                         conName (undefined :: t c a p)

--------------------------------------------------------------------------------

class ObjectWithSingleFieldEnc f where
    objectWithSingleFieldEnc :: Options -> f a -> Encoding

instance ( ObjectWithSingleFieldEnc a
         , ObjectWithSingleFieldEnc b ) => ObjectWithSingleFieldEnc (a :+: b) where
    objectWithSingleFieldEnc opts (L1 x) = objectWithSingleFieldEnc opts x
    objectWithSingleFieldEnc opts (R1 x) = objectWithSingleFieldEnc opts x

instance ( GToEncoding a, ConsToEncoding a
         , Constructor c ) => ObjectWithSingleFieldEnc (C1 c a) where
    objectWithSingleFieldEnc opts v =
      E.openCurly <>
      toEncoding
          (constructorTagModifier opts
          (conName (undefined :: t c a p))) <>
      E.colon <>
      gToEncoding opts v <>
      E.closeCurly

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------



-------------------------------------------------------------------------------
-- base
-------------------------------------------------------------------------------

instance ToJSON1 Identity where
    liftToJSON t _ (Identity a) = t a
    {-# INLINE liftToJSON #-}

    liftToEncoding t _ (Identity a) = t a
    {-# INLINE liftToEncoding #-}

instance (ToJSON a) => ToJSON (Identity a) where
    toJSON = toJSON1
    {-# INLINE toJSON #-}

    toEncoding = toEncoding1
    {-# INLINE toEncoding #-}


instance ToJSON2 Const where
    liftToJSON2 t _ _ _ (Const x) = t x
    {-# INLINE liftToJSON2 #-}

    liftToEncoding2 t _ _ _ (Const x) = t x
    {-# INLINE liftToEncoding2 #-}

instance ToJSON a => ToJSON1 (Const a) where
    liftToJSON _ _ (Const x) = toJSON x
    {-# INLINE liftToJSON #-}

    liftToEncoding _ _ (Const x) = toEncoding x
    {-# INLINE liftToEncoding #-}

instance ToJSON a => ToJSON (Const a b) where
    toJSON (Const x) = toJSON x
    {-# INLINE toJSON #-}

    toEncoding (Const x) = toEncoding x
    {-# INLINE toEncoding #-}


instance ToJSON1 Maybe where
    liftToJSON t _ (Just a) = t a
    liftToJSON _  _ Nothing  = Null
    {-# INLINE liftToJSON #-}

    liftToEncoding t _ (Just a) = t a
    liftToEncoding _  _ Nothing  = Encoding EB.null_
    {-# INLINE liftToEncoding #-}

instance (ToJSON a) => ToJSON (Maybe a) where
    toJSON = toJSON1
    {-# INLINE toJSON #-}

    toEncoding = toEncoding1
    {-# INLINE toEncoding #-}


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

left, right :: Text
left  = "Left"
right = "Right"


instance ToJSON Bool where
    toJSON = Bool
    {-# INLINE toJSON #-}

    toEncoding = Encoding . EB.bool
    {-# INLINE toEncoding #-}


instance ToJSON Ordering where
  toJSON     = toJSON     . orderingToText
  toEncoding = toEncoding . orderingToText

orderingToText :: Ordering -> T.Text
orderingToText o = case o of
                     LT -> "LT"
                     EQ -> "EQ"
                     GT -> "GT"

instance ToJSON () where
    toJSON _ = emptyArray
    {-# INLINE toJSON #-}

    toEncoding _ = emptyArray_
    {-# INLINE toEncoding #-}


instance ToJSON Char where
    toJSON = String . T.singleton
    {-# INLINE toJSON #-}

    toJSONList = String . T.pack
    {-# INLINE toJSONList #-}

    toEncoding = Encoding . EB.string . (:[])
    {-# INLINE toEncoding #-}

    toEncodingList = Encoding . EB.string
    {-# INLINE toEncodingList #-}


instance ToJSON Double where
    toJSON = realFloatToJSON
    {-# INLINE toJSON #-}

    toEncoding = realFloatToEncoding
    {-# INLINE toEncoding #-}


instance ToJSON Number where
    toJSON (D d) = toJSON d
    toJSON (I i) = toJSON i
    {-# INLINE toJSON #-}

    toEncoding (D d) = toEncoding d
    toEncoding (I i) = toEncoding i
    {-# INLINE toEncoding #-}


instance ToJSON Float where
    toJSON = realFloatToJSON
    {-# INLINE toJSON #-}

    toEncoding = realFloatToEncoding
    {-# INLINE toEncoding #-}


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


instance HasResolution a => ToJSON (Fixed a) where
    toJSON = Number . realToFrac
    {-# INLINE toJSON #-}

    toEncoding = Encoding . EB.number . realToFrac
    {-# INLINE toEncoding #-}


instance ToJSON Int where
    toJSON = Number . fromIntegral
    {-# INLINE toJSON #-}

    toEncoding = Encoding . B.intDec
    {-# INLINE toEncoding #-}

instance ToJSON Integer where
    toJSON = Number . fromInteger
    {-# INLINE toJSON #-}

    toEncoding = Encoding . B.integerDec
    {-# INLINE toEncoding #-}


instance ToJSON Natural where
    toJSON = toJSON . toInteger
    {-# INLINE toJSON #-}

    toEncoding = toEncoding . toInteger
    {-# INLINE toEncoding #-}


instance ToJSON Int8 where
    toJSON = Number . fromIntegral
    {-# INLINE toJSON #-}

    toEncoding = Encoding . B.int8Dec
    {-# INLINE toEncoding #-}


instance ToJSON Int16 where
    toJSON = Number . fromIntegral
    {-# INLINE toJSON #-}

    toEncoding = Encoding . B.int16Dec
    {-# INLINE toEncoding #-}


instance ToJSON Int32 where
    toJSON = Number . fromIntegral
    {-# INLINE toJSON #-}

    toEncoding = Encoding . B.int32Dec
    {-# INLINE toEncoding #-}


instance ToJSON Int64 where
    toJSON = Number . fromIntegral
    {-# INLINE toJSON #-}

    toEncoding = Encoding . B.int64Dec
    {-# INLINE toEncoding #-}


instance ToJSON Word where
    toJSON = Number . fromIntegral
    {-# INLINE toJSON #-}

    toEncoding = Encoding . B.wordDec
    {-# INLINE toEncoding #-}


instance ToJSON Word8 where
    toJSON = Number . fromIntegral
    {-# INLINE toJSON #-}

    toEncoding = Encoding . B.word8Dec
    {-# INLINE toEncoding #-}


instance ToJSON Word16 where
    toJSON = Number . fromIntegral
    {-# INLINE toJSON #-}

    toEncoding = Encoding . B.word16Dec
    {-# INLINE toEncoding #-}


instance ToJSON Word32 where
    toJSON = Number . fromIntegral
    {-# INLINE toJSON #-}

    toEncoding = Encoding . B.word32Dec
    {-# INLINE toEncoding #-}

instance ToJSON Word64 where
    toJSON = Number . fromIntegral
    {-# INLINE toJSON #-}

    toEncoding = Encoding . B.word64Dec
    {-# INLINE toEncoding #-}

instance ToJSON Text where
    toJSON = String
    {-# INLINE toJSON #-}

    toEncoding = Encoding . EB.text
    {-# INLINE toEncoding #-}

instance ToJSON LT.Text where
    toJSON = String . LT.toStrict
    {-# INLINE toJSON #-}

    toEncoding t = Encoding $
      B.char7 '"' <>
      LT.foldrChunks (\x xs -> EB.unquoted x <> xs) (B.char7 '"') t
    {-# INLINE toEncoding #-}

instance ToJSON Version where
    toJSON = toJSON . showVersion
    {-# INLINE toJSON #-}

    toEncoding = toEncoding . showVersion
    {-# INLINE toEncoding #-}


-------------------------------------------------------------------------------
-- semigroups NonEmpty
-------------------------------------------------------------------------------

instance ToJSON1 NonEmpty where
    liftToJSON t _ = listValue t . NE.toList
    {-# INLINE liftToJSON #-}

    liftToEncoding t _ = listEncoding t . NE.toList
    {-# INLINE liftToEncoding #-}

instance (ToJSON a) => ToJSON (NonEmpty a) where
    toJSON = toJSON1
    {-# INLINE toJSON #-}

    toEncoding = toEncoding1
    {-# INLINE toEncoding #-}

-------------------------------------------------------------------------------
-- scientific
-------------------------------------------------------------------------------

instance ToJSON Scientific where
    toJSON = Number
    {-# INLINE toJSON #-}

    toEncoding = Encoding . EB.number
    {-# INLINE toEncoding #-}

-------------------------------------------------------------------------------
-- containers
-------------------------------------------------------------------------------

instance ToJSON1 Seq.Seq where
    liftToJSON t _ = listValue t . toList
    {-# INLINE liftToJSON #-}

    liftToEncoding t _ = listEncoding t . toList
    {-# INLINE liftToEncoding #-}

instance (ToJSON a) => ToJSON (Seq.Seq a) where
    toJSON = toJSON1
    {-# INLINE toJSON #-}

    toEncoding = toEncoding1
    {-# INLINE toEncoding #-}


instance ToJSON1 Set.Set where
    liftToJSON t _ = listValue t . Set.toList
    {-# INLINE liftToJSON #-}

    liftToEncoding t _ = listEncoding t . Set.toList
    {-# INLINE liftToEncoding #-}

instance (ToJSON a) => ToJSON (Set.Set a) where
    toJSON = toJSON1
    {-# INLINE toJSON #-}

    toEncoding = toEncoding1
    {-# INLINE toEncoding #-}


instance ToJSON IntSet.IntSet where
    toJSON = toJSON . IntSet.toList
    {-# INLINE toJSON #-}

    toEncoding = toEncoding . IntSet.toList
    {-# INLINE toEncoding #-}


instance ToJSON1 IntMap.IntMap where
    liftToJSON t tol = liftToJSON to' tol' . IntMap.toList
      where
        to'  = liftToJSON2     toJSON toJSONList t tol
        tol' = liftToJSONList2 toJSON toJSONList t tol
    {-# INLINE liftToJSON #-}

    liftToEncoding t tol = liftToEncoding to' tol' . IntMap.toList
      where
        to'  = liftToEncoding2     toEncoding toEncodingList t tol
        tol' = liftToEncodingList2 toEncoding toEncodingList t tol
    {-# INLINE liftToEncoding #-}

instance ToJSON a => ToJSON (IntMap.IntMap a) where
    toJSON = toJSON1
    {-# INLINE toJSON #-}

    toEncoding = toEncoding1
    {-# INLINE toEncoding #-}


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


instance (ToJSON v, ToJSONKey k) => ToJSON (M.Map k v) where
    toJSON = toJSON1
    {-# INLINE toJSON #-}

    toEncoding = toEncoding1
    {-# INLINE toEncoding #-}


instance ToJSON1 Tree.Tree where
    liftToJSON t tol = go
      where
        go (Tree.Node root branches) =
            liftToJSON2 t tol to' tol' (root, branches)

        to' = liftToJSON go (listValue go)
        tol' = liftToJSONList go (listValue go)
    {-# INLINE liftToJSON #-}

    liftToEncoding t tol = go
      where
        go (Tree.Node root branches) =
            liftToEncoding2 t tol to' tol' (root, branches)

        to' = liftToEncoding go (listEncoding go)
        tol' = liftToEncodingList go (listEncoding go)
    {-# INLINE liftToEncoding #-}

instance (ToJSON v) => ToJSON (Tree.Tree v) where
    toJSON = toJSON1
    {-# INLINE toJSON #-}

    toEncoding = toEncoding1
    {-# INLINE toEncoding #-}


-------------------------------------------------------------------------------
-- vector
-------------------------------------------------------------------------------

instance ToJSON1 Vector where
    liftToJSON t _ = Array . V.map t
    {-# INLINE liftToJSON #-}

    liftToEncoding t _ xs
        | V.null xs = emptyArray_
        | otherwise = Encoding $
            B.char7 '[' <> fromEncoding (t (V.unsafeHead xs)) <>
            V.foldr go (B.char7 ']') (V.unsafeTail xs)
          where
            go v b = B.char7 ',' <> fromEncoding (t v) <> b
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

vectorToJSON :: (VG.Vector v a, ToJSON a) => v a -> Value
vectorToJSON = Array . V.map toJSON . V.convert
{-# INLINE vectorToJSON #-}

instance (Storable a, ToJSON a) => ToJSON (VS.Vector a) where
    toJSON = vectorToJSON
    {-# INLINE toJSON #-}

    toEncoding = encodeVector
    {-# INLINE toEncoding #-}


instance (VP.Prim a, ToJSON a) => ToJSON (VP.Vector a) where
    toJSON = vectorToJSON
    {-# INLINE toJSON #-}

    toEncoding = encodeVector
    {-# INLINE toEncoding #-}


instance (VG.Vector VU.Vector a, ToJSON a) => ToJSON (VU.Vector a) where
    toJSON = vectorToJSON
    {-# INLINE toJSON #-}

    toEncoding = encodeVector
    {-# INLINE toEncoding #-}

-------------------------------------------------------------------------------
-- unordered-containers
-------------------------------------------------------------------------------

instance ToJSON1 HashSet.HashSet where
    liftToJSON t _ = listValue t . HashSet.toList
    {-# INLINE liftToJSON #-}

    liftToEncoding t _ = listEncoding t . HashSet.toList
    {-# INLINE liftToEncoding #-}

instance (ToJSON a) => ToJSON (HashSet.HashSet a) where
    toJSON = toJSON1
    {-# INLINE toJSON #-}

    toEncoding = toEncoding1
    {-# INLINE toEncoding #-}


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

-------------------------------------------------------------------------------
-- aeson
-------------------------------------------------------------------------------

instance ToJSON Value where
    toJSON a = a
    {-# INLINE toJSON #-}

    toEncoding = Encoding . EB.encodeToBuilder
    {-# INLINE toEncoding #-}

instance ToJSON DotNetTime where
    toJSON = toJSON . dotNetTime

    toEncoding = toEncoding . dotNetTime

dotNetTime :: DotNetTime -> String
dotNetTime (DotNetTime t) = secs ++ formatMillis t ++ ")/"
  where secs  = formatTime defaultTimeLocale "/Date(%s" t

-------------------------------------------------------------------------------
-- time
-------------------------------------------------------------------------------

instance ToJSON Day where
    toJSON       = stringEncoding
    toEncoding z = Encoding (EB.quote $ EB.day z)


instance ToJSON TimeOfDay where
    toJSON       = stringEncoding
    toEncoding z = Encoding (EB.quote $ EB.timeOfDay z)


instance ToJSON LocalTime where
    toJSON       = stringEncoding
    toEncoding z = Encoding (EB.quote $ EB.localTime z)

instance ToJSON ZonedTime where
    toJSON = stringEncoding

    toEncoding z = Encoding (EB.quote $ EB.zonedTime z)

formatMillis :: (FormatTime t) => t -> String
formatMillis = take 3 . formatTime defaultTimeLocale "%q"

instance ToJSON UTCTime where
    toJSON = stringEncoding

    toEncoding t = Encoding (EB.quote $ EB.utcTime t)

-- | Encode something t a JSON string.
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

instance ToJSON NominalDiffTime where
    toJSON = Number . realToFrac
    {-# INLINE toJSON #-}

    toEncoding = Encoding . EB.number . realToFrac
    {-# INLINE toEncoding #-}

-------------------------------------------------------------------------------
-- base Monoid/Semigroup
-------------------------------------------------------------------------------

instance ToJSON1 Dual where
    liftToJSON t _ = t . getDual
    {-# INLINE liftToJSON #-}

    liftToEncoding t _ = t . getDual
    {-# INLINE liftToEncoding #-}

instance ToJSON a => ToJSON (Dual a) where
    toJSON = toJSON1
    {-# INLINE toJSON #-}

    toEncoding = toEncoding1
    {-# INLINE toEncoding #-}


instance ToJSON1 First where
    liftToJSON t to' = liftToJSON t to' . getFirst
    {-# INLINE liftToJSON #-}

    liftToEncoding t to' = liftToEncoding t to' . getFirst
    {-# INLINE liftToEncoding #-}

instance ToJSON a => ToJSON (First a) where
    toJSON = toJSON1
    {-# INLINE toJSON #-}

    toEncoding = toEncoding1
    {-# INLINE toEncoding #-}


instance ToJSON1 Last where
    liftToJSON t to' = liftToJSON t to' . getLast
    {-# INLINE liftToJSON #-}

    liftToEncoding t to' = liftToEncoding t to' . getLast
    {-# INLINE liftToEncoding #-}

instance ToJSON a => ToJSON (Last a) where
    toJSON = toJSON1
    {-# INLINE toJSON #-}

    toEncoding = toEncoding1
    {-# INLINE toEncoding #-}

-------------------------------------------------------------------------------
-- tagged
-------------------------------------------------------------------------------

instance ToJSON (Proxy a) where
    toJSON _ = Null
    {-# INLINE toJSON #-}

    toEncoding _ = Encoding EB.null_
    {-# INLINE toEncoding #-}


instance ToJSON1 (Tagged a) where
    liftToJSON t _ (Tagged x) = t x
    {-# INLINE liftToJSON #-}

    liftToEncoding t _ (Tagged x) = t x
    {-# INLINE liftToEncoding #-}

instance ToJSON b => ToJSON (Tagged a b) where
    toJSON = toJSON1
    {-# INLINE toJSON #-}

    toEncoding = toEncoding1
    {-# INLINE toEncoding #-}


instance ToJSONKey b => ToJSONKey (Tagged a b) where
    toJSONKey = contramapToJSONKeyFunction unTagged toJSONKey
    toJSONKeyList = contramapToJSONKeyFunction (fmap unTagged) toJSONKeyList

-------------------------------------------------------------------------------
-- Instances for converting t map keys
-------------------------------------------------------------------------------

instance ToJSONKey Text where
    toJSONKey = ToJSONKeyText (id,toEncoding)


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

instance (ToJSON a, ToJSON b) => ToJSONKey (a,b)
instance (ToJSON a, ToJSON b, ToJSON c) => ToJSONKey (a,b,c)
instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d) => ToJSONKey (a,b,c,d)

instance ToJSONKey Char where
    toJSONKey = ToJSONKeyText (T.singleton, toEncoding)
    toJSONKeyList = ToJSONKeyText (T.pack, toEncoding . T.pack)

instance (ToJSONKey a, ToJSON a) => ToJSONKey [a] where
    toJSONKey = toJSONKeyList

instance (ToJSONKey a, ToJSON a) => ToJSONKey (Identity a) where
    toJSONKey = contramapToJSONKeyFunction runIdentity toJSONKey

-------------------------------------------------------------------------------
-- Tuple instances, see tuple-instances-to.hs
-------------------------------------------------------------------------------

instance ToJSON2 ((,) ) where
    liftToJSON2 toA _ toB _ (a, b) = Array $ V.create $ do
        mv <- VM.unsafeNew 2
        VM.unsafeWrite mv 0 (toA a)
        VM.unsafeWrite mv 1 (toB b)
        return mv
    {-# INLINE liftToJSON2 #-}

    liftToEncoding2 toA _ toB _ (a, b) = tuple $
        toA a >*<
        toB b
    {-# INLINE liftToEncoding2 #-}

instance (ToJSON a) => ToJSON1 ((,) a) where
    liftToJSON = liftToJSON2 toJSON toJSONList
    {-# INLINE liftToJSON #-}
    liftToEncoding = liftToEncoding2 toEncoding toEncodingList
    {-# INLINE liftToEncoding #-}

instance (ToJSON a, ToJSON b) => ToJSON (a, b) where
    toJSON = toJSON2
    {-# INLINE toJSON #-}
    toEncoding = toEncoding2
    {-# INLINE toEncoding #-}

instance (ToJSON a) => ToJSON2 ((,,) a) where
    liftToJSON2 toB _ toC _ (a, b, c) = Array $ V.create $ do
        mv <- VM.unsafeNew 3
        VM.unsafeWrite mv 0 (toJSON a)
        VM.unsafeWrite mv 1 (toB b)
        VM.unsafeWrite mv 2 (toC c)
        return mv
    {-# INLINE liftToJSON2 #-}

    liftToEncoding2 toB _ toC _ (a, b, c) = tuple $
        toEncoding a >*<
        toB b >*<
        toC c
    {-# INLINE liftToEncoding2 #-}

instance (ToJSON a, ToJSON b) => ToJSON1 ((,,) a b) where
    liftToJSON = liftToJSON2 toJSON toJSONList
    {-# INLINE liftToJSON #-}
    liftToEncoding = liftToEncoding2 toEncoding toEncodingList
    {-# INLINE liftToEncoding #-}

instance (ToJSON a, ToJSON b, ToJSON c) => ToJSON (a, b, c) where
    toJSON = toJSON2
    {-# INLINE toJSON #-}
    toEncoding = toEncoding2
    {-# INLINE toEncoding #-}

instance (ToJSON a, ToJSON b) => ToJSON2 ((,,,) a b) where
    liftToJSON2 toC _ toD _ (a, b, c, d) = Array $ V.create $ do
        mv <- VM.unsafeNew 4
        VM.unsafeWrite mv 0 (toJSON a)
        VM.unsafeWrite mv 1 (toJSON b)
        VM.unsafeWrite mv 2 (toC c)
        VM.unsafeWrite mv 3 (toD d)
        return mv
    {-# INLINE liftToJSON2 #-}

    liftToEncoding2 toC _ toD _ (a, b, c, d) = tuple $
        toEncoding a >*<
        toEncoding b >*<
        toC c >*<
        toD d
    {-# INLINE liftToEncoding2 #-}

instance (ToJSON a, ToJSON b, ToJSON c) => ToJSON1 ((,,,) a b c) where
    liftToJSON = liftToJSON2 toJSON toJSONList
    {-# INLINE liftToJSON #-}
    liftToEncoding = liftToEncoding2 toEncoding toEncodingList
    {-# INLINE liftToEncoding #-}

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d) => ToJSON (a, b, c, d) where
    toJSON = toJSON2
    {-# INLINE toJSON #-}
    toEncoding = toEncoding2
    {-# INLINE toEncoding #-}

instance (ToJSON a, ToJSON b, ToJSON c) => ToJSON2 ((,,,,) a b c) where
    liftToJSON2 toD _ toE _ (a, b, c, d, e) = Array $ V.create $ do
        mv <- VM.unsafeNew 5
        VM.unsafeWrite mv 0 (toJSON a)
        VM.unsafeWrite mv 1 (toJSON b)
        VM.unsafeWrite mv 2 (toJSON c)
        VM.unsafeWrite mv 3 (toD d)
        VM.unsafeWrite mv 4 (toE e)
        return mv
    {-# INLINE liftToJSON2 #-}

    liftToEncoding2 toD _ toE _ (a, b, c, d, e) = tuple $
        toEncoding a >*<
        toEncoding b >*<
        toEncoding c >*<
        toD d >*<
        toE e
    {-# INLINE liftToEncoding2 #-}

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d) => ToJSON1 ((,,,,) a b c d) where
    liftToJSON = liftToJSON2 toJSON toJSONList
    {-# INLINE liftToJSON #-}
    liftToEncoding = liftToEncoding2 toEncoding toEncodingList
    {-# INLINE liftToEncoding #-}

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e) => ToJSON (a, b, c, d, e) where
    toJSON = toJSON2
    {-# INLINE toJSON #-}
    toEncoding = toEncoding2
    {-# INLINE toEncoding #-}

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d) => ToJSON2 ((,,,,,) a b c d) where
    liftToJSON2 toE _ toF _ (a, b, c, d, e, f) = Array $ V.create $ do
        mv <- VM.unsafeNew 6
        VM.unsafeWrite mv 0 (toJSON a)
        VM.unsafeWrite mv 1 (toJSON b)
        VM.unsafeWrite mv 2 (toJSON c)
        VM.unsafeWrite mv 3 (toJSON d)
        VM.unsafeWrite mv 4 (toE e)
        VM.unsafeWrite mv 5 (toF f)
        return mv
    {-# INLINE liftToJSON2 #-}

    liftToEncoding2 toE _ toF _ (a, b, c, d, e, f) = tuple $
        toEncoding a >*<
        toEncoding b >*<
        toEncoding c >*<
        toEncoding d >*<
        toE e >*<
        toF f
    {-# INLINE liftToEncoding2 #-}

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e) => ToJSON1 ((,,,,,) a b c d e) where
    liftToJSON = liftToJSON2 toJSON toJSONList
    {-# INLINE liftToJSON #-}
    liftToEncoding = liftToEncoding2 toEncoding toEncodingList
    {-# INLINE liftToEncoding #-}

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f) => ToJSON (a, b, c, d, e, f) where
    toJSON = toJSON2
    {-# INLINE toJSON #-}
    toEncoding = toEncoding2
    {-# INLINE toEncoding #-}

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e) => ToJSON2 ((,,,,,,) a b c d e) where
    liftToJSON2 toF _ toG _ (a, b, c, d, e, f, g) = Array $ V.create $ do
        mv <- VM.unsafeNew 7
        VM.unsafeWrite mv 0 (toJSON a)
        VM.unsafeWrite mv 1 (toJSON b)
        VM.unsafeWrite mv 2 (toJSON c)
        VM.unsafeWrite mv 3 (toJSON d)
        VM.unsafeWrite mv 4 (toJSON e)
        VM.unsafeWrite mv 5 (toF f)
        VM.unsafeWrite mv 6 (toG g)
        return mv
    {-# INLINE liftToJSON2 #-}

    liftToEncoding2 toF _ toG _ (a, b, c, d, e, f, g) = tuple $
        toEncoding a >*<
        toEncoding b >*<
        toEncoding c >*<
        toEncoding d >*<
        toEncoding e >*<
        toF f >*<
        toG g
    {-# INLINE liftToEncoding2 #-}

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f) => ToJSON1 ((,,,,,,) a b c d e f) where
    liftToJSON = liftToJSON2 toJSON toJSONList
    {-# INLINE liftToJSON #-}
    liftToEncoding = liftToEncoding2 toEncoding toEncodingList
    {-# INLINE liftToEncoding #-}

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, ToJSON g) => ToJSON (a, b, c, d, e, f, g) where
    toJSON = toJSON2
    {-# INLINE toJSON #-}
    toEncoding = toEncoding2
    {-# INLINE toEncoding #-}

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f) => ToJSON2 ((,,,,,,,) a b c d e f) where
    liftToJSON2 toG _ toH _ (a, b, c, d, e, f, g, h) = Array $ V.create $ do
        mv <- VM.unsafeNew 8
        VM.unsafeWrite mv 0 (toJSON a)
        VM.unsafeWrite mv 1 (toJSON b)
        VM.unsafeWrite mv 2 (toJSON c)
        VM.unsafeWrite mv 3 (toJSON d)
        VM.unsafeWrite mv 4 (toJSON e)
        VM.unsafeWrite mv 5 (toJSON f)
        VM.unsafeWrite mv 6 (toG g)
        VM.unsafeWrite mv 7 (toH h)
        return mv
    {-# INLINE liftToJSON2 #-}

    liftToEncoding2 toG _ toH _ (a, b, c, d, e, f, g, h) = tuple $
        toEncoding a >*<
        toEncoding b >*<
        toEncoding c >*<
        toEncoding d >*<
        toEncoding e >*<
        toEncoding f >*<
        toG g >*<
        toH h
    {-# INLINE liftToEncoding2 #-}

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, ToJSON g) => ToJSON1 ((,,,,,,,) a b c d e f g) where
    liftToJSON = liftToJSON2 toJSON toJSONList
    {-# INLINE liftToJSON #-}
    liftToEncoding = liftToEncoding2 toEncoding toEncodingList
    {-# INLINE liftToEncoding #-}

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, ToJSON g, ToJSON h) => ToJSON (a, b, c, d, e, f, g, h) where
    toJSON = toJSON2
    {-# INLINE toJSON #-}
    toEncoding = toEncoding2
    {-# INLINE toEncoding #-}

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, ToJSON g) => ToJSON2 ((,,,,,,,,) a b c d e f g) where
    liftToJSON2 toH _ toI _ (a, b, c, d, e, f, g, h, i) = Array $ V.create $ do
        mv <- VM.unsafeNew 9
        VM.unsafeWrite mv 0 (toJSON a)
        VM.unsafeWrite mv 1 (toJSON b)
        VM.unsafeWrite mv 2 (toJSON c)
        VM.unsafeWrite mv 3 (toJSON d)
        VM.unsafeWrite mv 4 (toJSON e)
        VM.unsafeWrite mv 5 (toJSON f)
        VM.unsafeWrite mv 6 (toJSON g)
        VM.unsafeWrite mv 7 (toH h)
        VM.unsafeWrite mv 8 (toI i)
        return mv
    {-# INLINE liftToJSON2 #-}

    liftToEncoding2 toH _ toI _ (a, b, c, d, e, f, g, h, i) = tuple $
        toEncoding a >*<
        toEncoding b >*<
        toEncoding c >*<
        toEncoding d >*<
        toEncoding e >*<
        toEncoding f >*<
        toEncoding g >*<
        toH h >*<
        toI i
    {-# INLINE liftToEncoding2 #-}

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, ToJSON g, ToJSON h) => ToJSON1 ((,,,,,,,,) a b c d e f g h) where
    liftToJSON = liftToJSON2 toJSON toJSONList
    {-# INLINE liftToJSON #-}
    liftToEncoding = liftToEncoding2 toEncoding toEncodingList
    {-# INLINE liftToEncoding #-}

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, ToJSON g, ToJSON h, ToJSON i) => ToJSON (a, b, c, d, e, f, g, h, i) where
    toJSON = toJSON2
    {-# INLINE toJSON #-}
    toEncoding = toEncoding2
    {-# INLINE toEncoding #-}

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, ToJSON g, ToJSON h) => ToJSON2 ((,,,,,,,,,) a b c d e f g h) where
    liftToJSON2 toI _ toJ _ (a, b, c, d, e, f, g, h, i, j) = Array $ V.create $ do
        mv <- VM.unsafeNew 10
        VM.unsafeWrite mv 0 (toJSON a)
        VM.unsafeWrite mv 1 (toJSON b)
        VM.unsafeWrite mv 2 (toJSON c)
        VM.unsafeWrite mv 3 (toJSON d)
        VM.unsafeWrite mv 4 (toJSON e)
        VM.unsafeWrite mv 5 (toJSON f)
        VM.unsafeWrite mv 6 (toJSON g)
        VM.unsafeWrite mv 7 (toJSON h)
        VM.unsafeWrite mv 8 (toI i)
        VM.unsafeWrite mv 9 (toJ j)
        return mv
    {-# INLINE liftToJSON2 #-}

    liftToEncoding2 toI _ toJ _ (a, b, c, d, e, f, g, h, i, j) = tuple $
        toEncoding a >*<
        toEncoding b >*<
        toEncoding c >*<
        toEncoding d >*<
        toEncoding e >*<
        toEncoding f >*<
        toEncoding g >*<
        toEncoding h >*<
        toI i >*<
        toJ j
    {-# INLINE liftToEncoding2 #-}

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, ToJSON g, ToJSON h, ToJSON i) => ToJSON1 ((,,,,,,,,,) a b c d e f g h i) where
    liftToJSON = liftToJSON2 toJSON toJSONList
    {-# INLINE liftToJSON #-}
    liftToEncoding = liftToEncoding2 toEncoding toEncodingList
    {-# INLINE liftToEncoding #-}

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, ToJSON g, ToJSON h, ToJSON i, ToJSON j) => ToJSON (a, b, c, d, e, f, g, h, i, j) where
    toJSON = toJSON2
    {-# INLINE toJSON #-}
    toEncoding = toEncoding2
    {-# INLINE toEncoding #-}

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, ToJSON g, ToJSON h, ToJSON i) => ToJSON2 ((,,,,,,,,,,) a b c d e f g h i) where
    liftToJSON2 toJ _ toK _ (a, b, c, d, e, f, g, h, i, j, k) = Array $ V.create $ do
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
        VM.unsafeWrite mv 9 (toJ j)
        VM.unsafeWrite mv 10 (toK k)
        return mv
    {-# INLINE liftToJSON2 #-}

    liftToEncoding2 toJ _ toK _ (a, b, c, d, e, f, g, h, i, j, k) = tuple $
        toEncoding a >*<
        toEncoding b >*<
        toEncoding c >*<
        toEncoding d >*<
        toEncoding e >*<
        toEncoding f >*<
        toEncoding g >*<
        toEncoding h >*<
        toEncoding i >*<
        toJ j >*<
        toK k
    {-# INLINE liftToEncoding2 #-}

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, ToJSON g, ToJSON h, ToJSON i, ToJSON j) => ToJSON1 ((,,,,,,,,,,) a b c d e f g h i j) where
    liftToJSON = liftToJSON2 toJSON toJSONList
    {-# INLINE liftToJSON #-}
    liftToEncoding = liftToEncoding2 toEncoding toEncodingList
    {-# INLINE liftToEncoding #-}

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, ToJSON g, ToJSON h, ToJSON i, ToJSON j, ToJSON k) => ToJSON (a, b, c, d, e, f, g, h, i, j, k) where
    toJSON = toJSON2
    {-# INLINE toJSON #-}
    toEncoding = toEncoding2
    {-# INLINE toEncoding #-}

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, ToJSON g, ToJSON h, ToJSON i, ToJSON j) => ToJSON2 ((,,,,,,,,,,,) a b c d e f g h i j) where
    liftToJSON2 toK _ toL _ (a, b, c, d, e, f, g, h, i, j, k, l) = Array $ V.create $ do
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
        VM.unsafeWrite mv 10 (toK k)
        VM.unsafeWrite mv 11 (toL l)
        return mv
    {-# INLINE liftToJSON2 #-}

    liftToEncoding2 toK _ toL _ (a, b, c, d, e, f, g, h, i, j, k, l) = tuple $
        toEncoding a >*<
        toEncoding b >*<
        toEncoding c >*<
        toEncoding d >*<
        toEncoding e >*<
        toEncoding f >*<
        toEncoding g >*<
        toEncoding h >*<
        toEncoding i >*<
        toEncoding j >*<
        toK k >*<
        toL l
    {-# INLINE liftToEncoding2 #-}

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, ToJSON g, ToJSON h, ToJSON i, ToJSON j, ToJSON k) => ToJSON1 ((,,,,,,,,,,,) a b c d e f g h i j k) where
    liftToJSON = liftToJSON2 toJSON toJSONList
    {-# INLINE liftToJSON #-}
    liftToEncoding = liftToEncoding2 toEncoding toEncodingList
    {-# INLINE liftToEncoding #-}

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, ToJSON g, ToJSON h, ToJSON i, ToJSON j, ToJSON k, ToJSON l) => ToJSON (a, b, c, d, e, f, g, h, i, j, k, l) where
    toJSON = toJSON2
    {-# INLINE toJSON #-}
    toEncoding = toEncoding2
    {-# INLINE toEncoding #-}

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, ToJSON g, ToJSON h, ToJSON i, ToJSON j, ToJSON k) => ToJSON2 ((,,,,,,,,,,,,) a b c d e f g h i j k) where
    liftToJSON2 toL _ toM _ (a, b, c, d, e, f, g, h, i, j, k, l, m) = Array $ V.create $ do
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
        VM.unsafeWrite mv 11 (toL l)
        VM.unsafeWrite mv 12 (toM m)
        return mv
    {-# INLINE liftToJSON2 #-}

    liftToEncoding2 toL _ toM _ (a, b, c, d, e, f, g, h, i, j, k, l, m) = tuple $
        toEncoding a >*<
        toEncoding b >*<
        toEncoding c >*<
        toEncoding d >*<
        toEncoding e >*<
        toEncoding f >*<
        toEncoding g >*<
        toEncoding h >*<
        toEncoding i >*<
        toEncoding j >*<
        toEncoding k >*<
        toL l >*<
        toM m
    {-# INLINE liftToEncoding2 #-}

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, ToJSON g, ToJSON h, ToJSON i, ToJSON j, ToJSON k, ToJSON l) => ToJSON1 ((,,,,,,,,,,,,) a b c d e f g h i j k l) where
    liftToJSON = liftToJSON2 toJSON toJSONList
    {-# INLINE liftToJSON #-}
    liftToEncoding = liftToEncoding2 toEncoding toEncodingList
    {-# INLINE liftToEncoding #-}

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, ToJSON g, ToJSON h, ToJSON i, ToJSON j, ToJSON k, ToJSON l, ToJSON m) => ToJSON (a, b, c, d, e, f, g, h, i, j, k, l, m) where
    toJSON = toJSON2
    {-# INLINE toJSON #-}
    toEncoding = toEncoding2
    {-# INLINE toEncoding #-}

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, ToJSON g, ToJSON h, ToJSON i, ToJSON j, ToJSON k, ToJSON l) => ToJSON2 ((,,,,,,,,,,,,,) a b c d e f g h i j k l) where
    liftToJSON2 toM _ toN _ (a, b, c, d, e, f, g, h, i, j, k, l, m, n) = Array $ V.create $ do
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
        VM.unsafeWrite mv 12 (toM m)
        VM.unsafeWrite mv 13 (toN n)
        return mv
    {-# INLINE liftToJSON2 #-}

    liftToEncoding2 toM _ toN _ (a, b, c, d, e, f, g, h, i, j, k, l, m, n) = tuple $
        toEncoding a >*<
        toEncoding b >*<
        toEncoding c >*<
        toEncoding d >*<
        toEncoding e >*<
        toEncoding f >*<
        toEncoding g >*<
        toEncoding h >*<
        toEncoding i >*<
        toEncoding j >*<
        toEncoding k >*<
        toEncoding l >*<
        toM m >*<
        toN n
    {-# INLINE liftToEncoding2 #-}

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, ToJSON g, ToJSON h, ToJSON i, ToJSON j, ToJSON k, ToJSON l, ToJSON m) => ToJSON1 ((,,,,,,,,,,,,,) a b c d e f g h i j k l m) where
    liftToJSON = liftToJSON2 toJSON toJSONList
    {-# INLINE liftToJSON #-}
    liftToEncoding = liftToEncoding2 toEncoding toEncodingList
    {-# INLINE liftToEncoding #-}

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, ToJSON g, ToJSON h, ToJSON i, ToJSON j, ToJSON k, ToJSON l, ToJSON m, ToJSON n) => ToJSON (a, b, c, d, e, f, g, h, i, j, k, l, m, n) where
    toJSON = toJSON2
    {-# INLINE toJSON #-}
    toEncoding = toEncoding2
    {-# INLINE toEncoding #-}

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, ToJSON g, ToJSON h, ToJSON i, ToJSON j, ToJSON k, ToJSON l, ToJSON m) => ToJSON2 ((,,,,,,,,,,,,,,) a b c d e f g h i j k l m) where
    liftToJSON2 toN _ toO _ (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) = Array $ V.create $ do
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
        VM.unsafeWrite mv 13 (toN n)
        VM.unsafeWrite mv 14 (toO o)
        return mv
    {-# INLINE liftToJSON2 #-}

    liftToEncoding2 toN _ toO _ (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) = tuple $
        toEncoding a >*<
        toEncoding b >*<
        toEncoding c >*<
        toEncoding d >*<
        toEncoding e >*<
        toEncoding f >*<
        toEncoding g >*<
        toEncoding h >*<
        toEncoding i >*<
        toEncoding j >*<
        toEncoding k >*<
        toEncoding l >*<
        toEncoding m >*<
        toN n >*<
        toO o
    {-# INLINE liftToEncoding2 #-}

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, ToJSON g, ToJSON h, ToJSON i, ToJSON j, ToJSON k, ToJSON l, ToJSON m, ToJSON n) => ToJSON1 ((,,,,,,,,,,,,,,) a b c d e f g h i j k l m n) where
    liftToJSON = liftToJSON2 toJSON toJSONList
    {-# INLINE liftToJSON #-}
    liftToEncoding = liftToEncoding2 toEncoding toEncodingList
    {-# INLINE liftToEncoding #-}

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, ToJSON g, ToJSON h, ToJSON i, ToJSON j, ToJSON k, ToJSON l, ToJSON m, ToJSON n, ToJSON o) => ToJSON (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) where
    toJSON = toJSON2
    {-# INLINE toJSON #-}
    toEncoding = toEncoding2
    {-# INLINE toEncoding #-}
