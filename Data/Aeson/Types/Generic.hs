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
import Data.Aeson.Encode.Builder (emptyArray_)
import Data.Aeson.Encode.Functions (builder)
import Data.Aeson.Types.Instances
import Data.Aeson.Types.Internal
import Data.Bits (unsafeShiftR)
import Data.ByteString.Builder as B
import Data.DList (DList, toList, empty)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text, pack, unpack)
import GHC.Generics
import qualified Data.HashMap.Strict as H
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<*>), (<$>), pure)
import Data.Monoid (mempty)
#endif

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
    gToEncoding _opts _ = emptyArray_

instance (ConsToEncoding a) => GToEncoding (C1 c a) where
    -- Constructors need to be encoded differently depending on whether they're
    -- a record or not. This distinction is made by 'consToEncoding':
    gToEncoding opts = Encoding . consToEncoding opts . unM1

instance ( EncodeProduct a, EncodeProduct b ) => GToEncoding (a :*: b) where
    -- Products are encoded to an array. Here we allocate a mutable vector of
    -- the same size as the product and write the product's elements to it using
    -- 'encodeProduct':
    gToEncoding opts p = Encoding $
                         B.char7 '[' <> encodeProduct opts p <> B.char7 ']'

instance ( AllNullary    (a :+: b) allNullary
         , SumToEncoding (a :+: b) allNullary ) => GToEncoding (a :+: b) where
    -- If all constructors of a sum datatype are nullary and the
    -- 'allNullaryToStringTag' option is set they are encoded to
    -- strings.  This distinction is made by 'sumToEncoding':
    gToEncoding opts = Encoding .
                       (unTagged :: Tagged allNullary B.Builder -> B.Builder) .
                       sumToEncoding opts

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
    sumToEncoding :: Options -> f a -> Tagged allNullary B.Builder

instance ( GetConName               f
         , TaggedObjectEnc          f
         , ObjectWithSingleFieldEnc f
         , TwoElemArrayEnc          f ) => SumToEncoding f True where
    sumToEncoding opts
        | allNullaryToStringTag opts = Tagged . builder .
                                       constructorTagModifier opts . getConName
        | otherwise = Tagged . nonAllNullarySumToEncoding opts

instance ( TwoElemArrayEnc          f
         , TaggedObjectEnc          f
         , ObjectWithSingleFieldEnc f ) => SumToEncoding f False where
    sumToEncoding opts = Tagged . nonAllNullarySumToEncoding opts

nonAllNullarySumToEncoding :: ( TwoElemArrayEnc          f
                              , TaggedObjectEnc          f
                              , ObjectWithSingleFieldEnc f
                              ) => Options -> f a -> B.Builder
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

instance (RecordToPairs f) => TaggedObjectPairs' f True where
    taggedObjectPairs' opts _ = Tagged . toList . recordToPairs opts

instance (GToJSON f) => TaggedObjectPairs' f False where
    taggedObjectPairs' opts contentsFieldName =
        Tagged . (:[]) . (pack contentsFieldName .=) . gToJSON opts

--------------------------------------------------------------------------------

class TaggedObjectEnc f where
    taggedObjectEnc :: Options -> String -> String -> f a -> B.Builder

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
        B.char7 '{' <>
        (builder tagFieldName <>
         B.char7 ':' <>
         builder (constructorTagModifier opts (conName (undefined :: t c a p)))) <>
        B.char7 ',' <>
        ((unTagged :: Tagged isRecord B.Builder -> B.Builder) .
         taggedObjectEnc' opts contentsFieldName . unM1 $ v) <>
        B.char7 '}'

class TaggedObjectEnc' f isRecord where
    taggedObjectEnc' :: Options -> String -> f a -> Tagged isRecord B.Builder

instance (RecordToEncoding f) => TaggedObjectEnc' f True where
    taggedObjectEnc' opts _ = Tagged . recordToEncoding opts

instance (GToEncoding f) => TaggedObjectEnc' f False where
    taggedObjectEnc' opts contentsFieldName =
        Tagged . (\z -> builder contentsFieldName <> B.char7 ':' <> z) .
        gbuilder opts

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
    twoElemArrayEnc :: Options -> f a -> B.Builder

instance (TwoElemArrayEnc a, TwoElemArrayEnc b) => TwoElemArrayEnc (a :+: b) where
    twoElemArrayEnc opts (L1 x) = twoElemArrayEnc opts x
    twoElemArrayEnc opts (R1 x) = twoElemArrayEnc opts x

instance ( GToEncoding a, ConsToEncoding a
         , Constructor c ) => TwoElemArrayEnc (C1 c a) where
    twoElemArrayEnc opts x = fromEncoding . tuple $
      builder (constructorTagModifier opts (conName (undefined :: t c a p))) >*<
      gbuilder opts x

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
    consToEncoding :: Options -> f a -> B.Builder

class ConsToEncoding' f isRecord where
    consToEncoding' :: Options -> Bool -- ^ Are we a record with one field?
                    -> f a -> Tagged isRecord B.Builder

instance ( IsRecord        f isRecord
         , ConsToEncoding' f isRecord ) => ConsToEncoding f where
    consToEncoding opts = (unTagged :: Tagged isRecord B.Builder -> B.Builder)
                          . consToEncoding' opts (isUnary (undefined :: f a))

instance (RecordToEncoding f) => ConsToEncoding' f True where
    consToEncoding' opts isUn x
      | (True,True) <- (unwrapUnaryRecords opts,isUn) = Tagged $   recordToEncoding opts x
      | otherwise = Tagged $
          B.char7 '{' <>
          recordToEncoding opts x <>
          B.char7 '}'

instance GToEncoding f => ConsToEncoding' f False where
    consToEncoding' opts _ = Tagged . gbuilder opts

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
                               , K1 Nothing <- k1 = empty
    recordToPairs opts m1 = fieldToPair opts m1

fieldToPair :: (Selector s, GToJSON a) => Options -> S1 s a p -> DList Pair
fieldToPair opts m1 = pure ( pack $ fieldLabelModifier opts $ selName m1
                           , gToJSON opts (unM1 m1)
                           )

--------------------------------------------------------------------------------

class RecordToEncoding f where
    recordToEncoding :: Options -> f a -> B.Builder

instance (RecordToEncoding a, RecordToEncoding b) => RecordToEncoding (a :*: b) where
    recordToEncoding opts (a :*: b) = recordToEncoding opts a <>
                                      B.char7 ',' <>
                                      recordToEncoding opts b

instance (Selector s, GToEncoding a) => RecordToEncoding (S1 s a) where
    recordToEncoding = fieldToEncoding

instance OVERLAPPING_ (Selector s, ToJSON a) =>
  RecordToEncoding (S1 s (K1 i (Maybe a))) where
    recordToEncoding opts (M1 k1) | omitNothingFields opts
                                  , K1 Nothing <- k1 = mempty
    recordToEncoding opts m1 = fieldToEncoding opts m1

fieldToEncoding :: (Selector s, GToEncoding a) => Options -> S1 s a p -> B.Builder
fieldToEncoding opts m1 =
    builder (fieldLabelModifier opts $ selName m1) <>
    B.char7 ':' <>
    gbuilder opts (unM1 m1)

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
    encodeProduct :: Options -> f a -> B.Builder

instance ( EncodeProduct a
         , EncodeProduct b ) => EncodeProduct (a :*: b) where
    encodeProduct opts (a :*: b) = encodeProduct opts a <>
                                   B.char7 ',' <>
                                   encodeProduct opts b

instance OVERLAPPABLE_ (GToEncoding a) => EncodeProduct a where
    encodeProduct opts = gbuilder opts

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
    objectWithSingleFieldEnc :: Options -> f a -> B.Builder

instance ( ObjectWithSingleFieldEnc a
         , ObjectWithSingleFieldEnc b ) => ObjectWithSingleFieldEnc (a :+: b) where
    objectWithSingleFieldEnc opts (L1 x) = objectWithSingleFieldEnc opts x
    objectWithSingleFieldEnc opts (R1 x) = objectWithSingleFieldEnc opts x

instance ( GToEncoding a, ConsToEncoding a
         , Constructor c ) => ObjectWithSingleFieldEnc (C1 c a) where
    objectWithSingleFieldEnc opts v =
      B.char7 '{' <>
      builder (constructorTagModifier opts
               (conName (undefined :: t c a p))) <>
      B.char7 ':' <>
      gbuilder opts v <>
      B.char7 '}'

gbuilder :: GToEncoding f => Options -> f a -> Builder
gbuilder opts = fromEncoding . gToEncoding opts

--------------------------------------------------------------------------------
-- Generic parseJSON

instance OVERLAPPABLE_ (GFromJSON a) => GFromJSON (M1 i c a) where
    -- Meta-information, which is not handled elsewhere, is just added to the
    -- parsed value:
    gParseJSON opts = fmap M1 . gParseJSON opts

instance (FromJSON a) => GFromJSON (K1 i a) where
    -- Constant values are decoded using their FromJSON instance:
    gParseJSON _opts = fmap K1 . parseJSON

instance GFromJSON U1 where
    -- Empty constructors are expected to be encoded as an empty array:
    gParseJSON _opts v
        | isEmptyArray v = pure U1
        | otherwise      = typeMismatch "unit constructor (U1)" v

instance (ConsFromJSON a) => GFromJSON (C1 c a) where
    -- Constructors need to be decoded differently depending on whether they're
    -- a record or not. This distinction is made by consParseJSON:
    gParseJSON opts = fmap M1 . consParseJSON opts

instance ( FromProduct a, FromProduct b
         , ProductSize a, ProductSize b ) => GFromJSON (a :*: b) where
    -- Products are expected to be encoded to an array. Here we check whether we
    -- got an array of the same size as the product, then parse each of the
    -- product's elements using parseProduct:
    gParseJSON opts = withArray "product (:*:)" $ \arr ->
      let lenArray = V.length arr
          lenProduct = (unTagged2 :: Tagged2 (a :*: b) Int -> Int)
                       productSize in
      if lenArray == lenProduct
      then parseProduct opts arr 0 lenProduct
      else fail $ "When expecting a product of " ++ show lenProduct ++
                  " values, encountered an Array of " ++ show lenArray ++
                  " elements instead"

instance ( AllNullary (a :+: b) allNullary
         , ParseSum   (a :+: b) allNullary ) => GFromJSON   (a :+: b) where
    -- If all constructors of a sum datatype are nullary and the
    -- 'allNullaryToStringTag' option is set they are expected to be
    -- encoded as strings.  This distinction is made by 'parseSum':
    gParseJSON opts = (unTagged :: Tagged allNullary (Parser ((a :+: b) d)) ->
                                                     (Parser ((a :+: b) d)))
                    . parseSum opts

--------------------------------------------------------------------------------

class ParseSum f allNullary where
    parseSum :: Options -> Value -> Tagged allNullary (Parser (f a))

instance ( SumFromString    (a :+: b)
         , FromPair         (a :+: b)
         , FromTaggedObject (a :+: b) ) => ParseSum (a :+: b) True where
    parseSum opts
        | allNullaryToStringTag opts = Tagged . parseAllNullarySum    opts
        | otherwise                  = Tagged . parseNonAllNullarySum opts

instance ( FromPair         (a :+: b)
         , FromTaggedObject (a :+: b) ) => ParseSum (a :+: b) False where
    parseSum opts = Tagged . parseNonAllNullarySum opts

--------------------------------------------------------------------------------

parseAllNullarySum :: SumFromString f => Options -> Value -> Parser (f a)
parseAllNullarySum opts = withText "Text" $ \key ->
                            maybe (notFound $ unpack key) return $
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

parseNonAllNullarySum :: ( FromPair                       (a :+: b)
                         , FromTaggedObject               (a :+: b)
                         ) => Options -> Value -> Parser ((a :+: b) c)
parseNonAllNullarySum opts =
    case sumEncoding opts of
      TaggedObject{..} ->
          withObject "Object" $ \obj -> do
            tag <- obj .: pack tagFieldName
            fromMaybe (notFound $ unpack tag) $
              parseFromTaggedObject opts contentsFieldName obj tag

      ObjectWithSingleField ->
          withObject "Object" $ \obj ->
            case H.toList obj of
              [pair@(tag, _)] -> fromMaybe (notFound $ unpack tag) $
                                   parsePair opts pair
              _ -> fail "Object doesn't have a single field"

      TwoElemArray ->
          withArray "Array" $ \arr ->
            if V.length arr == 2
            then case V.unsafeIndex arr 0 of
                   String tag -> fromMaybe (notFound $ unpack tag) $
                                   parsePair opts (tag, V.unsafeIndex arr 1)
                   _ -> fail "First element is not a String"
            else fail "Array doesn't have 2 elements"

--------------------------------------------------------------------------------

class FromTaggedObject f where
    parseFromTaggedObject :: Options -> String -> Object -> Text
                          -> Maybe (Parser (f a))

instance (FromTaggedObject a, FromTaggedObject b) =>
    FromTaggedObject (a :+: b) where
        parseFromTaggedObject opts contentsFieldName obj tag =
            (fmap L1 <$> parseFromTaggedObject opts contentsFieldName obj tag) <|>
            (fmap R1 <$> parseFromTaggedObject opts contentsFieldName obj tag)

instance ( FromTaggedObject' f
         , Constructor c ) => FromTaggedObject (C1 c f) where
    parseFromTaggedObject opts contentsFieldName obj tag
        | tag == name = Just $ M1 <$> parseFromTaggedObject'
                                        opts contentsFieldName obj
        | otherwise = Nothing
        where
          name = pack $ constructorTagModifier opts $
                          conName (undefined :: t c f p)

--------------------------------------------------------------------------------

class FromTaggedObject' f where
    parseFromTaggedObject' :: Options -> String -> Object -> Parser (f a)

class FromTaggedObject'' f isRecord where
    parseFromTaggedObject'' :: Options -> String -> Object
                            -> Tagged isRecord (Parser (f a))

instance ( IsRecord             f isRecord
         , FromTaggedObject''   f isRecord
         ) => FromTaggedObject' f where
    parseFromTaggedObject' opts contentsFieldName =
        (unTagged :: Tagged isRecord (Parser (f a)) -> Parser (f a)) .
        parseFromTaggedObject'' opts contentsFieldName

instance (FromRecord f) => FromTaggedObject'' f True where
    parseFromTaggedObject'' opts _ = Tagged . parseRecord opts Nothing

instance (GFromJSON f) => FromTaggedObject'' f False where
    parseFromTaggedObject'' opts contentsFieldName = Tagged .
      (gParseJSON opts <=< (.: pack contentsFieldName))

--------------------------------------------------------------------------------

class ConsFromJSON f where
    consParseJSON  :: Options -> Value -> Parser (f a)

class ConsFromJSON' f isRecord where
    consParseJSON' :: Options -> (Maybe Text) -- ^ A dummy label
                                           --   (Nothing to use proper label)
                   -> Value -> Tagged isRecord (Parser (f a))

instance ( IsRecord        f isRecord
         , ConsFromJSON'   f isRecord
         ) => ConsFromJSON f where
    consParseJSON opts v = let
      (v2,lab) = case (unwrapUnaryRecords opts,isUnary (undefined :: f a)) of
                       -- use a dummy object with a dummy label
        (True,True) -> ((object [(pack "dummy",v)]),Just $ pack "dummy")
        _ ->(v,Nothing)
      in (unTagged :: Tagged isRecord (Parser (f a)) -> Parser (f a))
                       $ consParseJSON' opts lab v2


instance (FromRecord f) => ConsFromJSON' f True where
    consParseJSON' opts mlab = Tagged . (withObject "record (:*:)"
                                $ parseRecord opts mlab)

instance (GFromJSON f) => ConsFromJSON' f False where
    consParseJSON' opts _ = Tagged . gParseJSON opts

--------------------------------------------------------------------------------

class FromRecord f where
    parseRecord :: Options -> (Maybe Text) -- ^ A dummy label
                                           --   (Nothing to use proper label)
                 -> Object -> Parser (f a)

instance (FromRecord a, FromRecord b) => FromRecord (a :*: b) where
    parseRecord opts _ obj = (:*:) <$> parseRecord opts Nothing obj
                                   <*> parseRecord opts Nothing obj

instance (Selector s, GFromJSON a) => FromRecord (S1 s a) where
    parseRecord opts (Just lab) = maybe (notFound $ unpack lab)
                      (gParseJSON opts) . H.lookup lab
    parseRecord opts Nothing    = maybe (notFound label)
                      (gParseJSON opts) . H.lookup (pack label)
        where
          label = fieldLabelModifier opts $ selName (undefined :: t s a p)

instance OVERLAPPING_ (Selector s, FromJSON a) =>
  FromRecord (S1 s (K1 i (Maybe a))) where
    parseRecord _ (Just lab) obj = (M1 . K1) <$> obj .:? lab
    parseRecord opts Nothing obj = (M1 . K1) <$> obj .:? pack label
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

class FromProduct f where
    parseProduct :: Options -> Array -> Int -> Int -> Parser (f a)

instance (FromProduct a, FromProduct b) => FromProduct (a :*: b) where
    parseProduct opts arr ix len =
        (:*:) <$> parseProduct opts arr ix  lenL
              <*> parseProduct opts arr ixR lenR
        where
          lenL = len `unsafeShiftR` 1
          ixR  = ix + lenL
          lenR = len - lenL

instance (GFromJSON a) => FromProduct (S1 s a) where
    parseProduct opts arr ix _ = gParseJSON opts $ V.unsafeIndex arr ix

--------------------------------------------------------------------------------

class FromPair f where
    parsePair :: Options -> Pair -> Maybe (Parser (f a))

instance (FromPair a, FromPair b) => FromPair (a :+: b) where
    parsePair opts pair = (fmap L1 <$> parsePair opts pair) <|>
                          (fmap R1 <$> parsePair opts pair)

instance (Constructor c, GFromJSON a, ConsFromJSON a) => FromPair (C1 c a) where
    parsePair opts (tag, value)
        | tag == tag' = Just $ gParseJSON opts value
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
instance AllNullary (K1 i c) False
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

notFound :: String -> Parser a
notFound key = fail $ "The key \"" ++ key ++ "\" was not found"
{-# INLINE notFound #-}
