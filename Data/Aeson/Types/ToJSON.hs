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
    , genericLiftToJSON
    , genericLiftToEncoding
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

-- We need internal here for generic deriving
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

import qualified Data.ByteString.Builder    as B (toLazyByteString)
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
import qualified Data.Tree                  as Tree
import qualified Data.Vector                as V
import qualified Data.Vector.Generic        as VG
import qualified Data.Vector.Mutable        as VM
import qualified Data.Vector.Primitive      as VP
import qualified Data.Vector.Storable       as VS
import qualified Data.Vector.Unboxed        as VU

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

-------------------------------------------------------------------------------
-- Generics
-------------------------------------------------------------------------------

-- | Class of generic representation types that can be converted to
-- JSON.
class GToJSON arity f where
    -- | This method (applied to 'defaultOptions') is used as the
    -- default generic implementation of 'toJSON' (if the @arity@ is 'Zero')
    -- or 'liftToJSON' (if the @arity@ is 'One').
    gToJSON :: Options -> Proxy arity
            -> (a -> Value) -> ([a] -> Value) -> f a -> Value

-- | Class of generic representation types that can be converted to
-- a JSON 'Encoding'.
class GToEncoding arity f where
    -- | This method (applied to 'defaultOptions') can be used as the
    -- default generic implementation of 'toEncoding' (if the @arity@ is 'Zero')
    -- or 'liftToEncoding' (if the @arity@ is 'One').
    gToEncoding :: Options -> Proxy arity
                -> (a -> Encoding) -> ([a] -> Encoding) -> f a -> Encoding

-- | A configurable generic JSON creator. This function applied to
-- 'defaultOptions' is used as the default for 'toJSON' when the type
-- is an instance of 'Generic'.
genericToJSON :: (Generic a, GToJSON Zero (Rep a))
              => Options -> a -> Value
genericToJSON opts = gToJSON opts proxyZero undefined undefined . from

-- | A configurable generic JSON creator. This function applied to
-- 'defaultOptions' is used as the default for 'liftToJSON' when the type
-- is an instance of 'Generic1'.
genericLiftToJSON :: (Generic1 f, GToJSON One (Rep1 f))
                  => Options -> (a -> Value) -> ([a] -> Value)
                  -> f a -> Value
genericLiftToJSON opts tj tjl = gToJSON opts proxyOne tj tjl . from1

-- | A configurable generic JSON encoder. This function applied to
-- 'defaultOptions' is used as the default for 'toEncoding' when the type
-- is an instance of 'Generic'.
genericToEncoding :: (Generic a, GToEncoding Zero (Rep a))
                  => Options -> a -> Encoding
genericToEncoding opts = gToEncoding opts proxyZero undefined undefined . from

-- | A configurable generic JSON encoder. This function applied to
-- 'defaultOptions' is used as the default for 'liftToEncoding' when the type
-- is an instance of 'Generic1'.
genericLiftToEncoding :: (Generic1 f, GToEncoding One (Rep1 f))
                      => Options -> (a -> Encoding) -> ([a] -> Encoding)
                      -> f a -> Encoding
genericLiftToEncoding opts te tel = gToEncoding opts proxyOne te tel . from1

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

    default toJSON :: (Generic a, GToJSON Zero (Rep a)) => a -> Value
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
    toEncoding = E.value . toJSON
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
    name .= value = E.pair name (toEncoding value)
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
--
-- Instead of manually writing your 'ToJSON1' instance, there are two options
-- to do it automatically:
--
-- * "Data.Aeson.TH" provides Template Haskell functions which will derive an
-- instance at compile time. The generated instance is optimized for your type
-- so will probably be more efficient than the following two options:
--
-- * The compiler can provide a default generic implementation for
-- 'toJSON1'.
--
-- To use the second, simply add a @deriving 'Generic1'@ clause to your
-- datatype and declare a 'ToJSON1' instance for your datatype without giving
-- definitions for 'liftToJSON' or 'liftToEncoding'.
--
-- For example:
--
-- @
-- {-\# LANGUAGE DeriveGeneric \#-}
--
-- import "GHC.Generics"
--
-- data Pair = Pair { pairFst :: a, pairSnd :: b } deriving 'Generic1'
--
-- instance ToJSON a => ToJSON1 (Pair a)
-- @
--
-- If @DefaultSignatures@ doesn't give exactly the results you want,
-- you can customize the generic encoding with only a tiny amount of
-- effort, using 'genericLiftToJSON' and 'genericLiftToEncoding' with
-- your preferred 'Options':
--
-- @
-- instance ToJSON a => ToJSON1 (Pair a) where
--     liftToJSON     = 'genericLiftToJSON' 'defaultOptions'
--     liftToEncoding = 'genericLiftToEncoding' 'defaultOptions'
-- @
class ToJSON1 f where
    liftToJSON :: (a -> Value) -> ([a] -> Value) -> f a -> Value

    default liftToJSON :: (Generic1 f, GToJSON One (Rep1 f))
                       => (a -> Value) -> ([a] -> Value) -> f a -> Value
    liftToJSON = genericLiftToJSON defaultOptions

    liftToJSONList :: (a -> Value) -> ([a] -> Value) -> [f a] -> Value
    liftToJSONList f g = listValue (liftToJSON f g)

    liftToEncoding :: (a -> Encoding) -> ([a] -> Encoding) -> f a -> Encoding

    default liftToEncoding :: (Generic1 f, GToEncoding One (Rep1 f))
                           => (a -> Encoding) -> ([a] -> Encoding)
                           -> f a -> Encoding
    liftToEncoding = genericLiftToEncoding defaultOptions

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
--
-- Instead of manually writing your 'ToJSON2' instance, "Data.Aeson.TH"
-- provides Template Haskell functions which will derive an instance at compile time.
--
-- The compiler cannot provide a default generic implementation for 'liftToJSON2',
-- unlike 'toJSON' and 'liftToJSON'.
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

instance OVERLAPPABLE_ (GToJSON arity a) => GToJSON arity (M1 i c a) where
    -- Meta-information, which is not handled elsewhere, is ignored:
    gToJSON opts pa tj tjl = gToJSON opts pa tj tjl . unM1

instance (ToJSON a) => GToJSON arity (K1 i a) where
    -- Constant values are encoded using their ToJSON instance:
    gToJSON _opts _ _ _ = toJSON . unK1

instance GToJSON One Par1 where
    -- Direct occurrences of the last type parameter are encoded with the
    -- function passed in as an argument:
    gToJSON _opts _ tj _ = tj . unPar1

instance (ToJSON1 f) => GToJSON One (Rec1 f) where
    -- Recursive occurrences of the last type parameter are encoded using their
    -- ToJSON1 instance:
    gToJSON _opts _ tj tjl = liftToJSON tj tjl . unRec1

instance GToJSON arity U1 where
    -- Empty constructors are encoded to an empty array:
    gToJSON _opts _ _ _ _ = emptyArray

instance (ConsToJSON arity a) => GToJSON arity (C1 c a) where
    -- Constructors need to be encoded differently depending on whether they're
    -- a record or not. This distinction is made by 'consToJSON':
    gToJSON opts pa tj tjl = consToJSON opts pa tj tjl . unM1

instance ( WriteProduct arity a, WriteProduct arity b
         , ProductSize        a, ProductSize        b
         ) => GToJSON arity (a :*: b) where
    -- Products are encoded to an array. Here we allocate a mutable vector of
    -- the same size as the product and write the product's elements to it using
    -- 'writeProduct':
    gToJSON opts pa tj tjl p =
        Array $ V.create $ do
          mv <- VM.unsafeNew lenProduct
          writeProduct opts pa mv 0 lenProduct tj tjl p
          return mv
        where
          lenProduct = (unTagged2 :: Tagged2 (a :*: b) Int -> Int)
                       productSize

instance ( AllNullary       (a :+: b) allNullary
         , SumToJSON  arity (a :+: b) allNullary
         ) => GToJSON arity (a :+: b) where
    -- If all constructors of a sum datatype are nullary and the
    -- 'allNullaryToStringTag' option is set they are encoded to
    -- strings.  This distinction is made by 'sumToJSON':
    gToJSON opts pa tj tjl = (unTagged :: Tagged allNullary Value -> Value)
                           . sumToJSON opts pa tj tjl

instance (ToJSON1 f, GToJSON One g) => GToJSON One (f :.: g) where
    -- If an occurrence of the last type parameter is nested inside two
    -- composed types, it is encoded by using the outermost type's ToJSON1
    -- instance to generically encode the innermost type:
    gToJSON opts pa tj tjl =
      let gtj = gToJSON opts pa tj tjl in
      liftToJSON gtj (listValue gtj) . unComp1

--------------------------------------------------------------------------------
-- Generic toEncoding

instance OVERLAPPABLE_ (GToEncoding arity a) => GToEncoding arity (M1 i c a) where
    -- Meta-information, which is not handled elsewhere, is ignored:
    gToEncoding opts pa te tel = gToEncoding opts pa te tel . unM1

instance (ToJSON a) => GToEncoding arity (K1 i a) where
    -- Constant values are encoded using their ToJSON instance:
    gToEncoding _opts _ _ _ = toEncoding . unK1

instance GToEncoding One Par1 where
    -- Direct occurrences of the last type parameter are encoded with the
    -- function passed in as an argument:
    gToEncoding _opts _ te _ = te . unPar1

instance (ToJSON1 f) => GToEncoding One (Rec1 f) where
    -- Recursive occurrences of the last type parameter are encoded using their
    -- ToEncoding1 instance:
    gToEncoding _opts _ te tel = liftToEncoding te tel . unRec1

instance GToEncoding arity U1 where
    -- Empty constructors are encoded to an empty array:
    gToEncoding _opts _ _ _ _ = E.emptyArray_

instance (ConsToEncoding arity a) => GToEncoding arity (C1 c a) where
    -- Constructors need to be encoded differently depending on whether they're
    -- a record or not. This distinction is made by 'consToEncoding':
    gToEncoding opts pa te tel = consToEncoding opts pa te tel . unM1

instance ( EncodeProduct  arity a
         , EncodeProduct  arity b
         ) => GToEncoding arity (a :*: b) where
    -- Products are encoded to an array. Here we allocate a mutable vector of
    -- the same size as the product and write the product's elements to it using
    -- 'encodeProduct':
    gToEncoding opts pa te tel p = E.tuple $ encodeProduct opts pa te tel p

instance ( AllNullary           (a :+: b) allNullary
         , SumToEncoding  arity (a :+: b) allNullary
         ) => GToEncoding arity (a :+: b) where
    -- If all constructors of a sum datatype are nullary and the
    -- 'allNullaryToStringTag' option is set they are encoded to
    -- strings.  This distinction is made by 'sumToEncoding':
    gToEncoding opts pa te tel
        = (unTagged :: Tagged allNullary Encoding -> Encoding)
        . sumToEncoding opts pa te tel

instance (ToJSON1 f, GToEncoding One g) => GToEncoding One (f :.: g) where
    -- If an occurrence of the last type parameter is nested inside two
    -- composed types, it is encoded by using the outermost type's ToJSON1
    -- instance to generically encode the innermost type:
    gToEncoding opts pa te tel =
      let gte = gToEncoding opts pa te tel in
      liftToEncoding gte (listEncoding gte) . unComp1

--------------------------------------------------------------------------------

class SumToJSON arity f allNullary where
    sumToJSON :: Options -> Proxy arity
              -> (a -> Value) -> ([a] -> Value)
              -> f a -> Tagged allNullary Value

instance ( GetConName                     f
         , TaggedObjectPairs        arity f
         , ObjectWithSingleFieldObj arity f
         , TwoElemArrayObj          arity f
         ) => SumToJSON arity f True where
    sumToJSON opts pa tj tjl
        | allNullaryToStringTag opts = Tagged . String . pack
                                     . constructorTagModifier opts . getConName
        | otherwise = Tagged . nonAllNullarySumToJSON opts pa tj tjl

instance ( TwoElemArrayObj          arity f
         , TaggedObjectPairs        arity f
         , ObjectWithSingleFieldObj arity f
         ) => SumToJSON arity f False where
    sumToJSON opts pa tj tjl = Tagged . nonAllNullarySumToJSON opts pa tj tjl

nonAllNullarySumToJSON :: ( TwoElemArrayObj          arity f
                          , TaggedObjectPairs        arity f
                          , ObjectWithSingleFieldObj arity f
                          ) => Options -> Proxy arity
                            -> (a -> Value) -> ([a] -> Value)
                            -> f a -> Value
nonAllNullarySumToJSON opts pa tj tjl =
    case sumEncoding opts of
      TaggedObject{..}      ->
        object . taggedObjectPairs opts pa tagFieldName contentsFieldName tj tjl
      ObjectWithSingleField -> Object . objectWithSingleFieldObj opts pa tj tjl
      TwoElemArray          -> Array  . twoElemArrayObj opts pa tj tjl

--------------------------------------------------------------------------------

class SumToEncoding arity f allNullary where
    sumToEncoding :: Options -> Proxy arity
                  -> (a -> Encoding) -> ([a] -> Encoding)
                  -> f a -> Tagged allNullary Encoding

instance ( GetConName                     f
         , TaggedObjectEnc          arity f
         , ObjectWithSingleFieldEnc arity f
         , TwoElemArrayEnc          arity f
         ) => SumToEncoding arity f True where
    sumToEncoding opts pa te tel
        | allNullaryToStringTag opts = Tagged . toEncoding .
                                       constructorTagModifier opts . getConName
        | otherwise = Tagged . nonAllNullarySumToEncoding opts pa te tel

instance ( TwoElemArrayEnc          arity f
         , TaggedObjectEnc          arity f
         , ObjectWithSingleFieldEnc arity f
         ) => SumToEncoding arity f False where
    sumToEncoding opts pa te tel = Tagged . nonAllNullarySumToEncoding opts pa te tel

nonAllNullarySumToEncoding :: ( TwoElemArrayEnc          arity f
                              , TaggedObjectEnc          arity f
                              , ObjectWithSingleFieldEnc arity f
                              ) => Options -> Proxy arity
                                -> (a -> Encoding) -> ([a] -> Encoding)
                                -> f a -> Encoding
nonAllNullarySumToEncoding opts pa te tel =
    case sumEncoding opts of
      TaggedObject{..}      ->
        taggedObjectEnc opts pa tagFieldName contentsFieldName te tel
      ObjectWithSingleField -> objectWithSingleFieldEnc opts pa te tel
      TwoElemArray          -> twoElemArrayEnc opts pa te tel

--------------------------------------------------------------------------------

class TaggedObjectPairs arity f where
    taggedObjectPairs :: Options -> Proxy arity
                      -> String -> String
                      -> (a -> Value) -> ([a] -> Value)
                      -> f a -> [Pair]

instance ( TaggedObjectPairs arity a
         , TaggedObjectPairs arity b
         ) => TaggedObjectPairs arity (a :+: b) where
    taggedObjectPairs opts pa tagFieldName contentsFieldName tj tjl (L1 x) =
        taggedObjectPairs opts pa tagFieldName contentsFieldName tj tjl x
    taggedObjectPairs opts pa tagFieldName contentsFieldName tj tjl (R1 x) =
        taggedObjectPairs opts pa tagFieldName contentsFieldName tj tjl x

instance ( IsRecord                 a isRecord
         , TaggedObjectPairs' arity a isRecord
         , Constructor c
         ) => TaggedObjectPairs arity (C1 c a) where
    taggedObjectPairs opts pa tagFieldName contentsFieldName tj tjl =
        (pack tagFieldName .= constructorTagModifier opts
                                 (conName (undefined :: t c a p)) :) .
        (unTagged :: Tagged isRecord [Pair] -> [Pair]) .
          taggedObjectPairs' opts pa contentsFieldName tj tjl . unM1

class TaggedObjectPairs' arity f isRecord where
    taggedObjectPairs' :: Options -> Proxy arity
                       -> String -> (a -> Value) -> ([a] -> Value)
                       -> f a -> Tagged isRecord [Pair]

instance OVERLAPPING_ TaggedObjectPairs' arity U1 False where
    taggedObjectPairs' _ _ _ _ _ _ = Tagged []

instance (RecordToPairs arity f) => TaggedObjectPairs' arity f True where
    taggedObjectPairs' opts pa _ tj tjl =
      Tagged . toList . recordToPairs opts pa tj tjl

instance (GToJSON arity f) => TaggedObjectPairs' arity f False where
    taggedObjectPairs' opts pa contentsFieldName tj tjl =
        Tagged . (:[]) . (pack contentsFieldName .=) . gToJSON opts pa tj tjl

--------------------------------------------------------------------------------

class TaggedObjectEnc arity f where
    taggedObjectEnc :: Options -> Proxy arity
                    -> String -> String
                    -> (a -> Encoding) -> ([a] -> Encoding)
                    -> f a -> Encoding

instance ( TaggedObjectEnc    arity a
         , TaggedObjectEnc    arity b
         ) => TaggedObjectEnc arity (a :+: b) where
    taggedObjectEnc opts pa tagFieldName contentsFieldName te tel (L1 x) =
        taggedObjectEnc opts pa tagFieldName contentsFieldName te tel x
    taggedObjectEnc opts pa tagFieldName contentsFieldName te tel (R1 x) =
        taggedObjectEnc opts pa tagFieldName contentsFieldName te tel x

instance ( IsRecord               a isRecord
         , TaggedObjectEnc' arity a isRecord
         , Constructor c
         ) => TaggedObjectEnc arity (C1 c a) where
    taggedObjectEnc opts pa tagFieldName contentsFieldName te tel v =
        E.openCurly <>
        (toEncoding tagFieldName <>
         E.colon <>
         toEncoding (constructorTagModifier opts (conName (undefined :: t c a p)))) <>
        ((unTagged :: Tagged isRecord Encoding -> Encoding) .
         taggedObjectEnc' opts pa contentsFieldName te tel . unM1 $ v) <>
        E.closeCurly

class TaggedObjectEnc' arity f isRecord where
    taggedObjectEnc' :: Options -> Proxy arity
                     -> String -> (a -> Encoding) -> ([a] -> Encoding)
                     -> f a -> Tagged isRecord Encoding

instance OVERLAPPING_ TaggedObjectEnc' arity U1 False where
    taggedObjectEnc' _ _ _ _ _ _ = Tagged mempty

instance (RecordToEncoding arity f) => TaggedObjectEnc' arity f True where
    taggedObjectEnc' opts pa _ te tel = Tagged . (E.comma <>) . fst
                                               . recordToEncoding opts pa te tel

instance (GToEncoding arity f) => TaggedObjectEnc' arity f False where
    taggedObjectEnc' opts pa contentsFieldName te tel =
        Tagged . (\z -> E.comma <> toEncoding contentsFieldName <> E.colon <> z) .
        gToEncoding opts pa te tel

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

class TwoElemArrayObj arity f where
    twoElemArrayObj :: Options -> Proxy arity
                    -> (a -> Value) -> ([a] -> Value)
                    -> f a -> V.Vector Value

instance ( TwoElemArrayObj arity a
         , TwoElemArrayObj arity b
         ) => TwoElemArrayObj arity (a :+: b) where
    twoElemArrayObj opts pa tj tjl (L1 x) = twoElemArrayObj opts pa tj tjl x
    twoElemArrayObj opts pa tj tjl (R1 x) = twoElemArrayObj opts pa tj tjl x

instance ( GToJSON    arity a
         , ConsToJSON arity a
         , Constructor c
         ) => TwoElemArrayObj arity (C1 c a) where
    twoElemArrayObj opts pa tj tjl x = V.create $ do
      mv <- VM.unsafeNew 2
      VM.unsafeWrite mv 0 $ String $ pack $ constructorTagModifier opts
                                   $ conName (undefined :: t c a p)
      VM.unsafeWrite mv 1 $ gToJSON opts pa tj tjl x
      return mv

--------------------------------------------------------------------------------

class TwoElemArrayEnc arity f where
    twoElemArrayEnc :: Options -> Proxy arity
                    -> (a -> Encoding) -> ([a] -> Encoding)
                    -> f a -> Encoding

instance ( TwoElemArrayEnc    arity a
         , TwoElemArrayEnc    arity b
         ) => TwoElemArrayEnc arity (a :+: b) where
    twoElemArrayEnc opts pa te tel (L1 x) = twoElemArrayEnc opts pa te tel x
    twoElemArrayEnc opts pa te tel (R1 x) = twoElemArrayEnc opts pa te tel x

instance ( GToEncoding    arity a
         , ConsToEncoding arity a
         , Constructor c
         ) => TwoElemArrayEnc arity (C1 c a) where
    twoElemArrayEnc opts pa te tel x = E.tuple $
      toEncoding (constructorTagModifier opts (conName (undefined :: t c a p))) >*<
      gToEncoding opts pa te tel x

--------------------------------------------------------------------------------

class ConsToJSON arity f where
    consToJSON     :: Options -> Proxy arity
                   -> (a -> Value) -> ([a] -> Value)
                   -> f a -> Value

class ConsToJSON' arity f isRecord where
    consToJSON'     :: Options -> Proxy arity
                    -> Bool -- ^ Are we a record with one field?
                    -> (a -> Value) -> ([a] -> Value)
                    -> f a -> Tagged isRecord Value

instance ( IsRecord          f isRecord
         , ConsToJSON' arity f isRecord
         ) => ConsToJSON arity f where
    consToJSON opts pa tj tjl =
        (unTagged :: Tagged isRecord Value -> Value)
      . consToJSON' opts pa (isUnary (undefined :: f a)) tj tjl

instance (RecordToPairs arity f) => ConsToJSON' arity f True where
    consToJSON' opts pa isUn tj tjl f = let
      vals = toList $ recordToPairs opts pa tj tjl f
      in case (unwrapUnaryRecords opts,isUn,vals) of
        (True,True,[(_,val)]) -> Tagged val
        _ -> Tagged $ object vals

instance GToJSON arity f => ConsToJSON' arity f False where
    consToJSON' opts pa _ tj tjl = Tagged . gToJSON opts pa tj tjl

--------------------------------------------------------------------------------

class ConsToEncoding arity f where
    consToEncoding :: Options -> Proxy arity
                   -> (a -> Encoding) -> ([a] -> Encoding)
                   -> f a -> Encoding

class ConsToEncoding' arity f isRecord where
    consToEncoding' :: Options -> Proxy arity
                    -> Bool -- ^ Are we a record with one field?
                    -> (a -> Encoding) -> ([a] -> Encoding)
                    -> f a -> Tagged isRecord Encoding

instance ( IsRecord                f isRecord
         , ConsToEncoding'   arity f isRecord
         ) => ConsToEncoding arity f where
    consToEncoding opts pa te tel =
        (unTagged :: Tagged isRecord Encoding -> Encoding)
      . consToEncoding' opts pa (isUnary (undefined :: f a)) te tel

instance (RecordToEncoding arity f) => ConsToEncoding' arity f True where
    consToEncoding' opts pa isUn te tel x =
      let (enc, mbVal) = recordToEncoding opts pa te tel x
      in case (unwrapUnaryRecords opts, isUn, mbVal) of
           (True, True, Just val) -> Tagged val
           _ -> Tagged $ E.wrapObject enc

instance GToEncoding arity f => ConsToEncoding' arity f False where
    consToEncoding' opts pa _ te tel = Tagged . gToEncoding opts pa te tel

--------------------------------------------------------------------------------

class RecordToPairs arity f where
    recordToPairs    :: Options -> Proxy arity
                     -> (a -> Value) -> ([a] -> Value)
                     -> f a -> DList Pair

instance ( RecordToPairs arity a
         , RecordToPairs arity b
         ) => RecordToPairs arity (a :*: b) where
    recordToPairs opts pa tj tjl (a :*: b) = recordToPairs opts pa tj tjl a <>
                                             recordToPairs opts pa tj tjl b

instance (Selector s, GToJSON arity a) => RecordToPairs arity (S1 s a) where
    recordToPairs = fieldToPair

instance OVERLAPPING_ (Selector s, ToJSON a) =>
  RecordToPairs arity (S1 s (K1 i (Maybe a))) where
    recordToPairs opts _ _ _ (M1 k1) | omitNothingFields opts
                                     , K1 Nothing <- k1 = DList.empty
    recordToPairs opts pa tj tjl m1 = fieldToPair opts pa tj tjl m1

fieldToPair :: (Selector s, GToJSON arity a)
            => Options -> Proxy arity
            -> (p -> Value) -> ([p] -> Value)
            -> S1 s a p -> DList Pair
fieldToPair opts pa tj tjl m1 = pure ( pack $ fieldLabelModifier opts $ selName m1
                                     , gToJSON opts pa tj tjl (unM1 m1)
                                     )

--------------------------------------------------------------------------------

class RecordToEncoding arity f where
    -- 1st element: whole thing
    -- 2nd element: in case the record has only 1 field, just the value
    --              of the field (without the key); 'Nothing' otherwise
    recordToEncoding :: Options -> Proxy arity
                     -> (a -> Encoding) -> ([a] -> Encoding)
                     -> f a -> (Encoding, Maybe Encoding)

instance ( RecordToEncoding    arity a
         , RecordToEncoding    arity b
         ) => RecordToEncoding arity (a :*: b) where
    recordToEncoding opts pa te tel (a :*: b) | omitNothingFields opts =
      (mconcat $ intersperse E.comma $
        filter (not . E.nullEncoding)
        [ fst (recordToEncoding opts pa te tel a)
        , fst (recordToEncoding opts pa te tel b) ]
      , Nothing)
    recordToEncoding opts pa te tel (a :*: b) =
      (fst (recordToEncoding opts pa te tel a) <> E.comma <>
       fst (recordToEncoding opts pa te tel b),
       Nothing)

instance (Selector s, GToEncoding arity a) => RecordToEncoding arity (S1 s a) where
    recordToEncoding = fieldToEncoding

instance OVERLAPPING_ (Selector s, ToJSON a) =>
  RecordToEncoding arity (S1 s (K1 i (Maybe a))) where
    recordToEncoding opts _ _ _ (M1 k1) | omitNothingFields opts
                                        , K1 Nothing <- k1 = (mempty, Nothing)
    recordToEncoding opts pa te tel m1 = fieldToEncoding opts pa te tel m1

fieldToEncoding :: (Selector s, GToEncoding arity a)
                => Options -> Proxy arity
                -> (p -> Encoding) -> ([p] -> Encoding)
                -> S1 s a p -> (Encoding, Maybe Encoding)
fieldToEncoding opts pa te tel m1 =
  let keyBuilder = toEncoding (fieldLabelModifier opts $ selName m1)
      valueBuilder = gToEncoding opts pa te tel (unM1 m1)
  in  (keyBuilder <> E.colon <> valueBuilder, Just valueBuilder)

--------------------------------------------------------------------------------

class WriteProduct arity f where
    writeProduct :: Options
                 -> Proxy arity
                 -> VM.MVector s Value
                 -> Int -- ^ index
                 -> Int -- ^ length
                 -> (a -> Value)
                 -> ([a] -> Value)
                 -> f a
                 -> ST s ()

instance ( WriteProduct arity a
         , WriteProduct arity b
         ) => WriteProduct arity (a :*: b) where
    writeProduct opts pa mv ix len tj tjl (a :*: b) = do
      writeProduct opts pa mv ix  lenL tj tjl a
      writeProduct opts pa mv ixR lenR tj tjl b
        where
          lenL = len `unsafeShiftR` 1
          lenR = len - lenL
          ixR  = ix  + lenL

instance OVERLAPPABLE_ (GToJSON arity a) => WriteProduct arity a where
    writeProduct opts pa mv ix _ tj tjl =
      VM.unsafeWrite mv ix . gToJSON opts pa tj tjl

--------------------------------------------------------------------------------

class EncodeProduct arity f where
    encodeProduct :: Options -> Proxy arity
                  -> (a -> Encoding) -> ([a] -> Encoding)
                  -> f a -> Encoding

instance ( EncodeProduct    arity a
         , EncodeProduct    arity b
         ) => EncodeProduct arity (a :*: b) where
    encodeProduct opts pa te tel (a :*: b) | omitNothingFields opts =
        mconcat $ intersperse E.comma $
        filter (not . E.nullEncoding)
        [encodeProduct opts pa te tel a, encodeProduct opts pa te tel b]
    encodeProduct opts pa te tel (a :*: b) = encodeProduct opts pa te tel a <>
                                             E.comma <>
                                             encodeProduct opts pa te tel b

instance OVERLAPPABLE_ (GToEncoding arity a) => EncodeProduct arity a where
    encodeProduct opts = gToEncoding opts

--------------------------------------------------------------------------------

class ObjectWithSingleFieldObj arity f where
    objectWithSingleFieldObj :: Options -> Proxy arity
                             -> (a -> Value) -> ([a] -> Value)
                             -> f a -> Object

instance ( ObjectWithSingleFieldObj arity a
         , ObjectWithSingleFieldObj arity b
         ) => ObjectWithSingleFieldObj arity (a :+: b) where
    objectWithSingleFieldObj opts pa tj tjl (L1 x) =
      objectWithSingleFieldObj opts pa tj tjl x
    objectWithSingleFieldObj opts pa tj tjl (R1 x) =
      objectWithSingleFieldObj opts pa tj tjl x

instance ( GToJSON    arity a
         , ConsToJSON arity a
         , Constructor c
         ) => ObjectWithSingleFieldObj arity (C1 c a) where
    objectWithSingleFieldObj opts pa tj tjl = H.singleton typ . gToJSON opts pa tj tjl
        where
          typ = pack $ constructorTagModifier opts $
                         conName (undefined :: t c a p)

--------------------------------------------------------------------------------

class ObjectWithSingleFieldEnc arity f where
    objectWithSingleFieldEnc :: Options -> Proxy arity
                             -> (a -> Encoding) -> ([a] -> Encoding)
                             -> f a -> Encoding

instance ( ObjectWithSingleFieldEnc    arity a
         , ObjectWithSingleFieldEnc    arity b
         ) => ObjectWithSingleFieldEnc arity (a :+: b) where
    objectWithSingleFieldEnc opts pa te tel (L1 x) =
      objectWithSingleFieldEnc opts pa te tel x
    objectWithSingleFieldEnc opts pa te tel (R1 x) =
      objectWithSingleFieldEnc opts pa te tel x

instance ( GToEncoding    arity a
         , ConsToEncoding arity a
         , Constructor c
         ) => ObjectWithSingleFieldEnc arity (C1 c a) where
    objectWithSingleFieldEnc opts pa te tel v =
      E.openCurly <>
      toEncoding
          (constructorTagModifier opts
          (conName (undefined :: t c a p))) <>
      E.colon <>
      gToEncoding opts pa te tel v <>
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
    liftToEncoding _  _ Nothing  = E.null_
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

    liftToEncoding2  toA _ _toB _ (Left a) = E.pairs $ E.pair "Left" $ toA a

    liftToEncoding2 _toA _ toB _ (Right b) = E.pairs $ E.pair "Right" $ toB b
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

    toEncoding = E.bool
    {-# INLINE toEncoding #-}

instance ToJSONKey Bool where
    toJSONKey = ToJSONKeyText
        ( (\x -> if x then "true" else "false")
        , (\x -> toEncoding $ if x then ("true" :: Text) else "false")
        )


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

    toEncoding = E.string . (:[])
    {-# INLINE toEncoding #-}

    toEncodingList = E.string
    {-# INLINE toEncodingList #-}


instance ToJSON Double where
    toJSON = realFloatToJSON
    {-# INLINE toJSON #-}

    toEncoding = E.double
    {-# INLINE toEncoding #-}

instance ToJSONKey Double where
    toJSONKey = toJSONKeyTextEnc E.doubleText
    {-# INLINE toJSONKey #-}


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

    toEncoding = E.float
    {-# INLINE toEncoding #-}

instance ToJSONKey Float where
    toJSONKey = toJSONKeyTextEnc E.floatText
    {-# INLINE toJSONKey #-}


instance (ToJSON a, Integral a) => ToJSON (Ratio a) where
    toJSON r = object [ "numerator"   .= numerator   r
                      , "denominator" .= denominator r
                      ]
    {-# INLINE toJSON #-}

    toEncoding r = E.pairs $
        "numerator" .= numerator r <>
        "denominator" .= denominator r
    {-# INLINE toEncoding #-}


instance HasResolution a => ToJSON (Fixed a) where
    toJSON = Number . realToFrac
    {-# INLINE toJSON #-}

    toEncoding = E.scientific . realToFrac
    {-# INLINE toEncoding #-}

instance HasResolution a => ToJSONKey (Fixed a) where
    toJSONKey = toJSONKeyTextEnc (E.scientificText . realToFrac)
    {-# INLINE toJSONKey #-}


instance ToJSON Int where
    toJSON = Number . fromIntegral
    {-# INLINE toJSON #-}

    toEncoding = E.int
    {-# INLINE toEncoding #-}

instance ToJSONKey Int where
    toJSONKey = toJSONKeyTextEnc E.intText
    {-# INLINE toJSONKey #-}


instance ToJSON Integer where
    toJSON = Number . fromInteger
    {-# INLINE toJSON #-}

    toEncoding = E.integer
    {-# INLINE toEncoding #-}

instance ToJSONKey Integer where
    toJSONKey = toJSONKeyTextEnc E.integerText
    {-# INLINE toJSONKey #-}


instance ToJSON Natural where
    toJSON = toJSON . toInteger
    {-# INLINE toJSON #-}

    toEncoding = toEncoding . toInteger
    {-# INLINE toEncoding #-}

instance ToJSONKey Natural where
    toJSONKey = toJSONKeyTextEnc (E.integerText . toInteger)
    {-# INLINE toJSONKey #-}


instance ToJSON Int8 where
    toJSON = Number . fromIntegral
    {-# INLINE toJSON #-}

    toEncoding = E.int8
    {-# INLINE toEncoding #-}

instance ToJSONKey Int8 where
    toJSONKey = toJSONKeyTextEnc E.int8Text
    {-# INLINE toJSONKey #-}


instance ToJSON Int16 where
    toJSON = Number . fromIntegral
    {-# INLINE toJSON #-}

    toEncoding = E.int16
    {-# INLINE toEncoding #-}

instance ToJSONKey Int16 where
    toJSONKey = toJSONKeyTextEnc E.int16Text
    {-# INLINE toJSONKey #-}


instance ToJSON Int32 where
    toJSON = Number . fromIntegral
    {-# INLINE toJSON #-}

    toEncoding = E.int32
    {-# INLINE toEncoding #-}

instance ToJSONKey Int32 where
    toJSONKey = toJSONKeyTextEnc E.int32Text
    {-# INLINE toJSONKey #-}


instance ToJSON Int64 where
    toJSON = Number . fromIntegral
    {-# INLINE toJSON #-}

    toEncoding = E.int64
    {-# INLINE toEncoding #-}

instance ToJSONKey Int64 where
    toJSONKey = toJSONKeyTextEnc E.int64Text
    {-# INLINE toJSONKey #-}


instance ToJSON Word where
    toJSON = Number . fromIntegral
    {-# INLINE toJSON #-}

    toEncoding = E.word
    {-# INLINE toEncoding #-}

instance ToJSONKey Word where
    toJSONKey = toJSONKeyTextEnc E.wordText
    {-# INLINE toJSONKey #-}


instance ToJSON Word8 where
    toJSON = Number . fromIntegral
    {-# INLINE toJSON #-}

    toEncoding = E.word8
    {-# INLINE toEncoding #-}

instance ToJSONKey Word8 where
    toJSONKey = toJSONKeyTextEnc E.word8Text
    {-# INLINE toJSONKey #-}


instance ToJSON Word16 where
    toJSON = Number . fromIntegral
    {-# INLINE toJSON #-}

    toEncoding = E.word16
    {-# INLINE toEncoding #-}

instance ToJSONKey Word16 where
    toJSONKey = toJSONKeyTextEnc E.word16Text
    {-# INLINE toJSONKey #-}


instance ToJSON Word32 where
    toJSON = Number . fromIntegral
    {-# INLINE toJSON #-}

    toEncoding = E.word32
    {-# INLINE toEncoding #-}

instance ToJSONKey Word32 where
    toJSONKey = toJSONKeyTextEnc E.word32Text
    {-# INLINE toJSONKey #-}


instance ToJSON Word64 where
    toJSON = Number . fromIntegral
    {-# INLINE toJSON #-}

    toEncoding = E.word64
    {-# INLINE toEncoding #-}

instance ToJSONKey Word64 where
    toJSONKey = toJSONKeyTextEnc E.word64Text
    {-# INLINE toJSONKey #-}


instance ToJSON Text where
    toJSON = String
    {-# INLINE toJSON #-}

    toEncoding = E.text
    {-# INLINE toEncoding #-}

instance ToJSONKey Text where
    toJSONKey = ToJSONKeyText (id, toEncoding)
    {-# INLINE toJSONKey #-}

instance ToJSON LT.Text where
    toJSON = String . LT.toStrict
    {-# INLINE toJSON #-}

    toEncoding = E.lazyText
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

    toEncoding = E.scientific
    {-# INLINE toEncoding #-}

instance ToJSONKey Scientific where
    toJSONKey = toJSONKeyTextEnc E.scientificText

-------------------------------------------------------------------------------
-- DList
-------------------------------------------------------------------------------

instance ToJSON1 DList.DList where
    liftToJSON t _ = listValue t . toList
    {-# INLINE liftToJSON #-}

    liftToEncoding t _ = listEncoding t . toList
    {-# INLINE liftToEncoding #-}

instance (ToJSON a) => ToJSON (DList.DList a) where
    toJSON = toJSON1
    {-# INLINE toJSON #-}

    toEncoding = toEncoding1
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

    liftToEncoding t _ =  listEncoding t . V.toList
    {-# INLINE liftToEncoding #-}

instance (ToJSON a) => ToJSON (Vector a) where
    toJSON = Array . V.map toJSON
    {-# INLINE toJSON #-}

    toEncoding = encodeVector
    {-# INLINE toEncoding #-}

encodeVector :: (ToJSON a, VG.Vector v a) => v a -> Encoding
encodeVector = listEncoding toEncoding . VG.toList
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

    toEncoding = E.value
    {-# INLINE toEncoding #-}

instance ToJSON DotNetTime where
    toJSON = toJSON . dotNetTime

    toEncoding = toEncoding . dotNetTime

dotNetTime :: DotNetTime -> String
dotNetTime (DotNetTime t) = secs ++ formatMillis t ++ ")/"
  where secs  = formatTime defaultTimeLocale "/Date(%s" t

formatMillis :: (FormatTime t) => t -> String
formatMillis = take 3 . formatTime defaultTimeLocale "%q"

-------------------------------------------------------------------------------
-- time
-------------------------------------------------------------------------------

instance ToJSON Day where
    toJSON     = stringEncoding . E.day
    toEncoding = E.day

instance ToJSONKey Day where
    toJSONKey = toJSONKeyTextEnc E.day


instance ToJSON TimeOfDay where
    toJSON     = stringEncoding . E.timeOfDay
    toEncoding = E.timeOfDay

instance ToJSONKey TimeOfDay where
    toJSONKey = toJSONKeyTextEnc E.timeOfDay


instance ToJSON LocalTime where
    toJSON     = stringEncoding . E.localTime
    toEncoding = E.localTime

instance ToJSONKey LocalTime where
    toJSONKey = toJSONKeyTextEnc E.localTime


instance ToJSON ZonedTime where
    toJSON     = stringEncoding . E.zonedTime
    toEncoding = E.zonedTime

instance ToJSONKey ZonedTime where
    toJSONKey = toJSONKeyTextEnc E.zonedTime


instance ToJSON UTCTime where
    toJSON     = stringEncoding . E.utcTime
    toEncoding = E.utcTime

instance ToJSONKey UTCTime where
    toJSONKey = toJSONKeyTextEnc E.utcTime

-- | Encode something t a JSON string.
stringEncoding :: Encoding -> Value
stringEncoding = String
    . T.dropAround (== '"')
    . T.decodeLatin1
    . L.toStrict
    . B.toLazyByteString
    . fromEncoding
{-# INLINE stringEncoding #-}

instance ToJSON NominalDiffTime where
    toJSON = Number . realToFrac
    {-# INLINE toJSON #-}

    toEncoding = E.scientific . realToFrac
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

    toEncoding _ = E.null_
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

-- | TODO: Move ToJSONKEyFunction to .Types(.Internal),
-- export and document this function from there
toJSONKeyTextEnc :: (a -> Encoding) -> ToJSONKeyFunction a
toJSONKeyTextEnc e = ToJSONKeyText
    -- TODO: dropAround is also used in stringEncoding, which is unfortunate atm
    ( T.dropAround (== '"') . T.decodeLatin1 . L.toStrict . B.toLazyByteString . fromEncoding . e
    , e
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
