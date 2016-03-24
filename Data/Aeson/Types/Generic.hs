{-# LANGUAGE CPP, DefaultSignatures, EmptyDataDecls, FlexibleInstances,
    FunctionalDependencies, KindSignatures,
    ScopedTypeVariables, TypeOperators, UndecidableInstances,
    ViewPatterns, NamedFieldPuns, FlexibleContexts, PatternGuards,
    RecordWildCards, DataKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

#include "overlapping-compat.h"

-- |
-- Module:      Data.Aeson.Types.Generic
-- Copyright:   (c) 2012-2016 Bryan O'Sullivan
--              (c) 2011, 2012 Bas Van Dijk
--              (c) 2011 MailRank, Inc.
-- License:     BSD3
-- Maintainer:  Bryan O'Sullivan <bos@serpentine.com>
-- Stability:   experimental
-- Portability: portable
--
-- Types for working with JSON data.

module Data.Aeson.Types.Generic ( ) where

import Control.Applicative ((<|>))
import Control.Monad ((<=<))
import Control.Monad.ST (ST)
import Data.Aeson.Encoding.Internal (Encoding, (>*<))
import qualified Data.Aeson.Encoding.Internal as E
import Data.Aeson.Types.Instances
import Data.Aeson.Types.Internal
import Data.Bits (unsafeShiftR)
import Data.DList (DList, toList, empty)
import Data.List (intersperse)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Proxy (Proxy(..))
import Data.Text (Text, pack, unpack)
import GHC.Generics
import qualified Data.HashMap.Strict as H
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<*>), (<$>), pure)
import Data.Monoid (mempty, mconcat)
#endif

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
                                     , K1 Nothing <- k1 = empty
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

--------------------------------------------------------------------------------
-- Generic parseJSON

instance OVERLAPPABLE_ (GFromJSON arity a) => GFromJSON arity (M1 i c a) where
    -- Meta-information, which is not handled elsewhere, is just added to the
    -- parsed value:
    gParseJSON opts pa pj pjl = fmap M1 . gParseJSON opts pa pj pjl

instance (FromJSON a) => GFromJSON arity (K1 i a) where
    -- Constant values are decoded using their FromJSON instance:
    gParseJSON _opts _ _ _ = fmap K1 . parseJSON

instance GFromJSON One Par1 where
    -- Direct occurrences of the last type parameter are decoded with the
    -- function passed in as an argument:
    gParseJSON _opts _ pj _ = fmap Par1 . pj

instance (FromJSON1 f) => GFromJSON One (Rec1 f) where
    -- Recursive occurrences of the last type parameter are decoded using their
    -- FromJSON1 instance:
    gParseJSON _opts _ pj pjl = fmap Rec1 . liftParseJSON pj pjl

instance GFromJSON arity U1 where
    -- Empty constructors are expected to be encoded as an empty array:
    gParseJSON _opts _ _ _ v
        | isEmptyArray v = pure U1
        | otherwise      = typeMismatch "unit constructor (U1)" v

instance (ConsFromJSON arity a) => GFromJSON arity (C1 c a) where
    -- Constructors need to be decoded differently depending on whether they're
    -- a record or not. This distinction is made by consParseJSON:
    gParseJSON opts pa pj pjl = fmap M1 . consParseJSON opts pa pj pjl

instance ( FromProduct arity a, FromProduct arity b
         , ProductSize       a, ProductSize       b
         ) => GFromJSON arity (a :*: b) where
    -- Products are expected to be encoded to an array. Here we check whether we
    -- got an array of the same size as the product, then parse each of the
    -- product's elements using parseProduct:
    gParseJSON opts pa pj pjl = withArray "product (:*:)" $ \arr ->
      let lenArray = V.length arr
          lenProduct = (unTagged2 :: Tagged2 (a :*: b) Int -> Int)
                       productSize in
      if lenArray == lenProduct
      then parseProduct opts pa arr 0 lenProduct pj pjl
      else fail $ "When expecting a product of " ++ show lenProduct ++
                  " values, encountered an Array of " ++ show lenArray ++
                  " elements instead"

instance ( AllNullary         (a :+: b) allNullary
         , ParseSum     arity (a :+: b) allNullary
         ) => GFromJSON arity (a :+: b) where
    -- If all constructors of a sum datatype are nullary and the
    -- 'allNullaryToStringTag' option is set they are expected to be
    -- encoded as strings.  This distinction is made by 'parseSum':
    gParseJSON opts pa pj pjl =
      (unTagged :: Tagged allNullary (Parser ((a :+: b) d)) ->
                                     (Parser ((a :+: b) d)))
                 . parseSum opts pa pj pjl

instance (FromJSON1 f, GFromJSON One g) => GFromJSON One (f :.: g) where
    -- If an occurrence of the last type parameter is nested inside two
    -- composed types, it is decoded by using the outermost type's FromJSON1
    -- instance to generically decode the innermost type:
    gParseJSON opts pa pj pjl =
      let gpj = gParseJSON opts pa pj pjl in
      fmap Comp1 . liftParseJSON gpj (listParser gpj)

--------------------------------------------------------------------------------

class ParseSum arity f allNullary where
    parseSum :: Options -> Proxy arity
             -> (Value -> Parser a) -> (Value -> Parser [a])
             -> Value -> Tagged allNullary (Parser (f a))

instance ( SumFromString          (a :+: b)
         , FromPair         arity (a :+: b)
         , FromTaggedObject arity (a :+: b)
         ) => ParseSum      arity (a :+: b) True where
    parseSum opts pa pj pjl
        | allNullaryToStringTag opts = Tagged . parseAllNullarySum    opts
        | otherwise                  = Tagged . parseNonAllNullarySum opts pa pj pjl

instance ( FromPair         arity (a :+: b)
         , FromTaggedObject arity (a :+: b)
         ) => ParseSum      arity (a :+: b) False where
    parseSum opts pa pj pjl = Tagged . parseNonAllNullarySum opts pa pj pjl

--------------------------------------------------------------------------------

parseAllNullarySum :: SumFromString f => Options -> Value -> Parser (f a)
parseAllNullarySum opts = withText "Text" $ \key ->
                            maybe (notFound key) return $
                              parseSumFromString opts key

class SumFromString f where
    parseSumFromString :: Options -> Text -> Maybe (f a)

instance (SumFromString a, SumFromString b) => SumFromString (a :+: b) where
    parseSumFromString opts key = (L1 <$> parseSumFromString opts key) <|>
                                  (R1 <$> parseSumFromString opts key)

instance (Constructor c) => SumFromString (C1 c U1) where
    parseSumFromString opts key | key == name = Just $ M1 U1
                                | otherwise   = Nothing
        where
          name = pack $ constructorTagModifier opts $
                          conName (undefined :: t c U1 p)

--------------------------------------------------------------------------------

parseNonAllNullarySum :: ( FromPair         arity (a :+: b)
                         , FromTaggedObject arity (a :+: b)
                         ) => Options -> Proxy arity
                           -> (Value -> Parser c) -> (Value -> Parser [c])
                           -> Value -> Parser ((a :+: b) c)
parseNonAllNullarySum opts pa pj pjl =
    case sumEncoding opts of
      TaggedObject{..} ->
          withObject "Object" $ \obj -> do
            tag <- obj .: pack tagFieldName
            fromMaybe (notFound tag) $
              parseFromTaggedObject opts pa contentsFieldName obj pj pjl tag

      ObjectWithSingleField ->
          withObject "Object" $ \obj ->
            case H.toList obj of
              [pair@(tag, _)] -> fromMaybe (notFound tag) $
                                   parsePair opts pa pj pjl pair
              _ -> fail "Object doesn't have a single field"

      TwoElemArray ->
          withArray "Array" $ \arr ->
            if V.length arr == 2
            then case V.unsafeIndex arr 0 of
                   String tag -> fromMaybe (notFound tag) $
                                   parsePair opts pa pj pjl (tag, V.unsafeIndex arr 1)
                   _ -> fail "First element is not a String"
            else fail "Array doesn't have 2 elements"

--------------------------------------------------------------------------------

class FromTaggedObject arity f where
    parseFromTaggedObject :: Options -> Proxy arity
                          -> String -> Object
                          -> (Value -> Parser a) -> (Value -> Parser [a])
                          -> Text -> Maybe (Parser (f a))

instance ( FromTaggedObject arity a, FromTaggedObject arity b) =>
    FromTaggedObject arity (a :+: b) where
        parseFromTaggedObject opts pa contentsFieldName obj pj pjl tag =
            (fmap L1 <$> parseFromTaggedObject opts pa contentsFieldName obj pj pjl tag) <|>
            (fmap R1 <$> parseFromTaggedObject opts pa contentsFieldName obj pj pjl tag)

instance ( FromTaggedObject' arity f
         , Constructor c
         ) => FromTaggedObject arity (C1 c f) where
    parseFromTaggedObject opts pa contentsFieldName obj pj pjl tag
        | tag == name = Just $ M1 <$> parseFromTaggedObject'
                                        opts pa contentsFieldName pj pjl obj
        | otherwise = Nothing
        where
          name = pack $ constructorTagModifier opts $
                          conName (undefined :: t c f p)

--------------------------------------------------------------------------------

class FromTaggedObject' arity f where
    parseFromTaggedObject' :: Options -> Proxy arity -> String
                           -> (Value -> Parser a) -> (Value -> Parser [a])
                           -> Object -> Parser (f a)

class FromTaggedObject'' arity f isRecord where
    parseFromTaggedObject'' :: Options -> Proxy arity -> String
                            -> (Value -> Parser a) -> (Value -> Parser [a])
                            -> Object -> Tagged isRecord (Parser (f a))

instance ( IsRecord                   f isRecord
         , FromTaggedObject''   arity f isRecord
         ) => FromTaggedObject' arity f where
    parseFromTaggedObject' opts pa contentsFieldName pj pjl =
        (unTagged :: Tagged isRecord (Parser (f a)) -> Parser (f a)) .
        parseFromTaggedObject'' opts pa contentsFieldName pj pjl

instance (FromRecord arity f) => FromTaggedObject'' arity f True where
    parseFromTaggedObject'' opts pa _ pj pjl =
      Tagged . parseRecord opts pa Nothing pj pjl

instance (GFromJSON arity f) => FromTaggedObject'' arity f False where
    parseFromTaggedObject'' opts pa contentsFieldName pj pjl = Tagged .
      (gParseJSON opts pa pj pjl <=< (.: pack contentsFieldName))

instance OVERLAPPING_ FromTaggedObject'' arity U1 False where
    parseFromTaggedObject'' _ _ _ _ _ _ = Tagged (pure U1)

--------------------------------------------------------------------------------

class ConsFromJSON arity f where
    consParseJSON  :: Options -> Proxy arity
                   -> (Value -> Parser a) -> (Value -> Parser [a])
                   -> Value -> Parser (f a)

class ConsFromJSON' arity f isRecord where
    consParseJSON' :: Options -> Proxy arity
                   -> (Maybe Text) -- ^ A dummy label
                                   --   (Nothing to use proper label)
                   -> (Value -> Parser a) -> (Value -> Parser [a])
                   -> Value -> Tagged isRecord (Parser (f a))

instance ( IsRecord            f isRecord
         , ConsFromJSON' arity f isRecord
         ) => ConsFromJSON arity f where
    consParseJSON opts pa pj pjl v = let
      (v2,lab) = case (unwrapUnaryRecords opts,isUnary (undefined :: f a)) of
                       -- use a dummy object with a dummy label
        (True,True) -> ((object [(pack "dummy",v)]),Just $ pack "dummy")
        _ ->(v,Nothing)
      in (unTagged :: Tagged isRecord (Parser (f a)) -> Parser (f a))
                       $ consParseJSON' opts pa lab pj pjl v2


instance (FromRecord arity f) => ConsFromJSON' arity f True where
    consParseJSON' opts pa mlab pj pjl = Tagged . (withObject "record (:*:)"
                                          $ parseRecord opts pa mlab pj pjl)

instance (GFromJSON arity f) => ConsFromJSON' arity f False where
    consParseJSON' opts pa _ pj pjl = Tagged . gParseJSON opts pa pj pjl

--------------------------------------------------------------------------------

class FromRecord arity f where
    parseRecord :: Options -> Proxy arity
                -> (Maybe Text) -- ^ A dummy label
                                --   (Nothing to use proper label)
                -> (Value -> Parser a) -> (Value -> Parser [a])
                -> Object -> Parser (f a)

instance ( FromRecord arity a
         , FromRecord arity b
         ) => FromRecord arity (a :*: b) where
    parseRecord opts pa _ pj pjl obj =
      (:*:) <$> parseRecord opts pa Nothing pj pjl obj
            <*> parseRecord opts pa Nothing pj pjl obj

instance ( Selector s
         , GFromJSON arity a
         ) => FromRecord arity (S1 s a) where
    parseRecord opts pa lab pj pjl =
      (<?> Key label) . gParseJSON opts pa pj pjl <=< (.: label)
        where
          label = fromMaybe defLabel lab
          defLabel = pack . fieldLabelModifier opts $
                       selName (undefined :: t s a p)

instance OVERLAPPING_ (Selector s, FromJSON a) =>
  FromRecord arity (S1 s (K1 i (Maybe a))) where
    parseRecord _ _ (Just lab) _ _ obj = (M1 . K1) <$> obj .:? lab
    parseRecord opts _ Nothing _ _ obj = (M1 . K1) <$> obj .:? pack label
        where
          label = fieldLabelModifier opts $
                    selName (undefined :: t s (K1 i (Maybe a)) p)

--------------------------------------------------------------------------------

class ProductSize f where
    productSize :: Tagged2 f Int

instance (ProductSize a, ProductSize b) => ProductSize (a :*: b) where
    productSize = Tagged2 $ unTagged2 (productSize :: Tagged2 a Int) +
                            unTagged2 (productSize :: Tagged2 b Int)

instance ProductSize (S1 s a) where
    productSize = Tagged2 1

--------------------------------------------------------------------------------

class FromProduct arity f where
    parseProduct :: Options -> Proxy arity
                 -> Array -> Int -> Int
                 -> (Value -> Parser a) -> (Value -> Parser [a])
                 -> Parser (f a)

instance ( FromProduct    arity a
         , FromProduct    arity b
         ) => FromProduct arity (a :*: b) where
    parseProduct opts pa arr ix len pj pjl =
        (:*:) <$> parseProduct opts pa arr ix  lenL pj pjl
              <*> parseProduct opts pa arr ixR lenR pj pjl
        where
          lenL = len `unsafeShiftR` 1
          ixR  = ix + lenL
          lenR = len - lenL

instance (GFromJSON arity a) => FromProduct arity (S1 s a) where
    parseProduct opts pa arr ix _ pj pjl =
      gParseJSON opts pa pj pjl $ V.unsafeIndex arr ix

--------------------------------------------------------------------------------

class FromPair arity f where
    parsePair :: Options -> Proxy arity
              -> (Value -> Parser a) -> (Value -> Parser [a])
              -> Pair -> Maybe (Parser (f a))

instance ( FromPair arity a
         , FromPair arity b
         ) => FromPair arity (a :+: b) where
    parsePair opts pa pj pjl pair = (fmap L1 <$> parsePair opts pa pj pjl pair) <|>
                                    (fmap R1 <$> parsePair opts pa pj pjl pair)

instance ( Constructor c
         , GFromJSON    arity a
         , ConsFromJSON arity a
         ) => FromPair arity (C1 c a) where
    parsePair opts pa pj pjl (tag, value)
        | tag == tag' = Just $ gParseJSON opts pa pj pjl value
        | otherwise   = Nothing
        where
          tag' = pack $ constructorTagModifier opts $
                          conName (undefined :: t c a p)

--------------------------------------------------------------------------------

class IsRecord (f :: * -> *) isRecord | f -> isRecord
  where
    isUnary :: f a -> Bool
    isUnary = const True

instance (IsRecord f isRecord) => IsRecord (f :*: g) isRecord
  where isUnary = const False
#if MIN_VERSION_base(4,9,0)
instance OVERLAPPING_ IsRecord (M1 S ('MetaSel 'Nothing u ss ds) f) False
#else
instance OVERLAPPING_ IsRecord (M1 S NoSelector f) False
#endif
instance (IsRecord f isRecord) => IsRecord (M1 S c f) isRecord
instance IsRecord (K1 i c) True
instance IsRecord Par1 True
instance IsRecord (f :.: g) True
instance IsRecord U1 False
  where isUnary = const False

--------------------------------------------------------------------------------

class AllNullary (f :: * -> *) allNullary | f -> allNullary

instance ( AllNullary a allNullaryL
         , AllNullary b allNullaryR
         , And allNullaryL allNullaryR allNullary
         ) => AllNullary (a :+: b) allNullary
instance AllNullary a allNullary => AllNullary (M1 i c a) allNullary
instance AllNullary (a :*: b) False
instance AllNullary (a :.: b) False
instance AllNullary (K1 i c) False
instance AllNullary Par1 False
instance AllNullary U1 True

--------------------------------------------------------------------------------

data True
data False

class    And bool1 bool2 bool3 | bool1 bool2 -> bool3

instance And True  True  True
instance And False False False
instance And False True  False
instance And True  False False

--------------------------------------------------------------------------------

newtype Tagged s b = Tagged {unTagged :: b}

newtype Tagged2 (s :: * -> *) b = Tagged2 {unTagged2 :: b}

--------------------------------------------------------------------------------

notFound :: Text -> Parser a
notFound key = fail $ "The key \"" ++ unpack key ++ "\" was not found"
{-# INLINE notFound #-}
