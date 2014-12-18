{-# LANGUAGE DefaultSignatures, EmptyDataDecls, FlexibleInstances,
    FunctionalDependencies, KindSignatures, OverlappingInstances,
    ScopedTypeVariables, TypeOperators, UndecidableInstances,
    ViewPatterns, NamedFieldPuns, FlexibleContexts, PatternGuards,
    RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module:      Data.Aeson.Types.Generic
-- Copyright:   (c) 2012-2015 Bryan O'Sullivan
--              (c) 2011, 2012 Bas Van Dijk
--              (c) 2011 MailRank, Inc.
-- License:     Apache
-- Maintainer:  Bryan O'Sullivan <bos@serpentine.com>
-- Stability:   experimental
-- Portability: portable
--
-- Types for working with JSON data.

module Data.Aeson.Types.Generic ( ) where

import Control.Applicative ((<*>), (<$>), (<|>), pure)
import Control.Monad ((<=<), join)
import Control.Monad.ST (ST)
import Data.Aeson.Encode.Builder (emptyArray_)
import Data.Aeson.Encode.Functions (builder)
import Data.Aeson.Types.Instances
import Data.Aeson.Types.Internal
import Data.Bits (unsafeShiftR)
import Data.ByteString.Builder as B
import Data.DList (DList, toList, empty)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>), mempty)
import Data.Text (Text, pack, unpack)
import GHC.Generics
import qualified Data.HashMap.Strict as H
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

--------------------------------------------------------------------------------
-- Generic toJSON

instance (GToJSON a) => GToJSON (M1 i c a) where
    -- Meta-information, which is not handled elsewhere, is ignored:
    gToJSON opts = gToJSON opts . unM1
    {-# INLINE gToJSON #-}

    gToEncoding opts = gToEncoding opts . unM1
    {-# INLINE gToEncoding #-}

instance (ToJSON a) => GToJSON (K1 i a) where
    -- Constant values are encoded using their ToJSON instance:
    gToJSON _opts = toJSON . unK1
    {-# INLINE gToJSON #-}

    gToEncoding _opts = toEncoding . unK1
    {-# INLINE gToEncoding #-}

instance GToJSON U1 where
    -- Empty constructors are encoded to an empty array:
    gToJSON _opts _ = emptyArray
    {-# INLINE gToJSON #-}

    gToEncoding _opts _ = emptyArray_
    {-# INLINE gToEncoding #-}

instance (ConsToJSON a) => GToJSON (C1 c a) where
    -- Constructors need to be encoded differently depending on whether they're
    -- a record or not. This distinction is made by 'consToJSON':
    gToJSON opts = consToJSON opts . unM1
    {-# INLINE gToJSON #-}

    gToEncoding opts = Encoding . consToEncoding opts . unM1
    {-# INLINE gToEncoding #-}

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
    {-# INLINE gToJSON #-}

    gToEncoding opts p = Encoding $
                         B.char7 '[' <> encodeProduct opts p <> B.char7 ']'

instance ( AllNullary (a :+: b) allNullary
         , SumToJSON  (a :+: b) allNullary ) => GToJSON (a :+: b) where
    -- If all constructors of a sum datatype are nullary and the
    -- 'allNullaryToStringTag' option is set they are encoded to
    -- strings.  This distinction is made by 'sumToJSON':
    gToJSON opts = (unTagged :: Tagged allNullary Value -> Value)
                 . sumToJSON opts
    {-# INLINE gToJSON #-}

    gToEncoding opts = Encoding .
                       (unTagged :: Tagged allNullary B.Builder -> B.Builder) .
                       sumToEncoding opts
    {-# INLINE gToEncoding #-}

--------------------------------------------------------------------------------

class SumToJSON f allNullary where
    sumToJSON :: Options -> f a -> Tagged allNullary Value
    sumToEncoding :: Options -> f a -> Tagged allNullary B.Builder

instance ( GetConName            f
         , TaggedObject     f
         , ObjectWithSingleField f
         , TwoElemArray          f ) => SumToJSON f True where
    sumToJSON opts
        | allNullaryToStringTag opts = Tagged . String . pack
                                     . constructorTagModifier opts . getConName
        | otherwise = Tagged . nonAllNullarySumToJSON opts
    {-# INLINE sumToJSON #-}

    sumToEncoding opts
        | allNullaryToStringTag opts = Tagged . builder .
                                       constructorTagModifier opts . getConName
        | otherwise = Tagged . nonAllNullarySumToEncoding opts
    {-# INLINE sumToEncoding #-}

instance ( TwoElemArray          f
         , TaggedObject          f
         , ObjectWithSingleField f ) => SumToJSON f False where
    sumToJSON opts = Tagged . nonAllNullarySumToJSON opts
    {-# INLINE sumToJSON #-}
    sumToEncoding opts = Tagged . nonAllNullarySumToEncoding opts
    {-# INLINE sumToEncoding #-}

nonAllNullarySumToJSON :: ( TwoElemArray          f
                          , TaggedObject          f
                          , ObjectWithSingleField f
                          ) => Options -> f a -> Value
nonAllNullarySumToJSON opts =
    case sumEncoding opts of
      TaggedObject{..}      ->
        object . taggedObjectPairs opts tagFieldName contentsFieldName
      ObjectWithSingleField -> Object . objectWithSingleFieldObj opts
      TwoElemArray          -> Array  . twoElemArrayObj opts
{-# INLINE nonAllNullarySumToJSON #-}

nonAllNullarySumToEncoding :: ( TwoElemArray          f
                          , TaggedObject          f
                          , ObjectWithSingleField f
                          ) => Options -> f a -> B.Builder
nonAllNullarySumToEncoding opts =
    case sumEncoding opts of
      TaggedObject{..}      ->
        taggedObjectEnc opts tagFieldName contentsFieldName
      ObjectWithSingleField -> objectWithSingleFieldEnc opts
      TwoElemArray          -> twoElemArrayEnc opts
{-# INLINE nonAllNullarySumToEncoding #-}

--------------------------------------------------------------------------------

class TaggedObject f where
    taggedObjectPairs :: Options -> String -> String -> f a -> [Pair]
    taggedObjectEnc :: Options -> String -> String -> f a -> B.Builder

instance ( TaggedObject a
         , TaggedObject b ) => TaggedObject (a :+: b) where
    taggedObjectPairs opts tagFieldName contentsFieldName (L1 x) =
        taggedObjectPairs opts tagFieldName contentsFieldName     x
    taggedObjectPairs opts tagFieldName contentsFieldName (R1 x) =
        taggedObjectPairs opts tagFieldName contentsFieldName     x
    {-# INLINE taggedObjectPairs #-}

    taggedObjectEnc opts tagFieldName contentsFieldName (L1 x) =
        taggedObjectEnc opts tagFieldName contentsFieldName     x
    taggedObjectEnc opts tagFieldName contentsFieldName (R1 x) =
        taggedObjectEnc opts tagFieldName contentsFieldName     x
    {-# INLINE taggedObjectEnc #-}

instance ( IsRecord      a isRecord
         , TaggedObject' a isRecord
         , Constructor c ) => TaggedObject (C1 c a) where
    taggedObjectPairs opts tagFieldName contentsFieldName =
        (pack tagFieldName .= constructorTagModifier opts
                                 (conName (undefined :: t c a p)) :) .
        (unTagged :: Tagged isRecord [Pair] -> [Pair]) .
          taggedObjectPairs' opts contentsFieldName . unM1
    {-# INLINE taggedObjectPairs #-}

    taggedObjectEnc opts tagFieldName contentsFieldName v =
        B.char7 '{' <>
        (builder tagFieldName <>
         B.char7 ':' <>
         builder (constructorTagModifier opts (conName (undefined :: t c a p)))) <>
        B.char7 ',' <>
        ((unTagged :: Tagged isRecord B.Builder -> B.Builder) .
         taggedObjectEnc' opts contentsFieldName . unM1 $ v) <>
        B.char7 '}'
    {-# INLINE taggedObjectEnc #-}

class TaggedObject' f isRecord where
    taggedObjectPairs' :: Options -> String -> f a -> Tagged isRecord [Pair]
    taggedObjectEnc' :: Options -> String -> f a -> Tagged isRecord B.Builder

instance (RecordTo f) => TaggedObject' f True where
    taggedObjectPairs' opts _ = Tagged . toList . recordToPairs opts
    {-# INLINE taggedObjectPairs' #-}

    taggedObjectEnc' opts _ = Tagged . recordToEncoding opts
    {-# INLINE taggedObjectEnc' #-}

instance (GToJSON f) => TaggedObject' f False where
    taggedObjectPairs' opts contentsFieldName =
        Tagged . (:[]) . (pack contentsFieldName .=) . gToJSON opts
    {-# INLINE taggedObjectPairs' #-}

    taggedObjectEnc' opts contentsFieldName =
        Tagged . (\z -> builder contentsFieldName <> B.char7 ':' <> z) .
        gbuilder opts
    {-# INLINE taggedObjectEnc' #-}

--------------------------------------------------------------------------------

-- | Get the name of the constructor of a sum datatype.
class GetConName f where
    getConName :: f a -> String

instance (GetConName a, GetConName b) => GetConName (a :+: b) where
    getConName (L1 x) = getConName x
    getConName (R1 x) = getConName x
    {-# INLINE getConName #-}

instance (Constructor c, GToJSON a, ConsToJSON a) => GetConName (C1 c a) where
    getConName = conName
    {-# INLINE getConName #-}

--------------------------------------------------------------------------------

class TwoElemArray f where
    twoElemArrayObj :: Options -> f a -> V.Vector Value
    twoElemArrayEnc :: Options -> f a -> B.Builder

instance (TwoElemArray a, TwoElemArray b) => TwoElemArray (a :+: b) where
    twoElemArrayObj opts (L1 x) = twoElemArrayObj opts x
    twoElemArrayObj opts (R1 x) = twoElemArrayObj opts x
    {-# INLINE twoElemArrayObj #-}

    twoElemArrayEnc opts (L1 x) = twoElemArrayEnc opts x
    twoElemArrayEnc opts (R1 x) = twoElemArrayEnc opts x
    {-# INLINE twoElemArrayEnc #-}

instance ( GToJSON a, ConsToJSON a
         , Constructor c ) => TwoElemArray (C1 c a) where
    twoElemArrayObj opts x = V.create $ do
      mv <- VM.unsafeNew 2
      VM.unsafeWrite mv 0 $ String $ pack $ constructorTagModifier opts
                                   $ conName (undefined :: t c a p)
      VM.unsafeWrite mv 1 $ gToJSON opts x
      return mv
    {-# INLINE twoElemArrayObj #-}

    twoElemArrayEnc opts x = fromEncoding . tuple $
      builder (constructorTagModifier opts (conName (undefined :: t c a p))) >*<
      gbuilder opts x
    {-# INLINE twoElemArrayEnc #-}

--------------------------------------------------------------------------------

class ConsToJSON f where
    consToJSON     :: Options -> f a -> Value
    consToEncoding :: Options -> f a -> B.Builder

class ConsToJSON' f isRecord where
    consToJSON'     :: Options -> Bool -- ^ Are we a record with one field?
                    -> f a -> Tagged isRecord Value
    consToEncoding' :: Options -> Bool -- ^ Are we a record with one field?
                    -> f a -> Tagged isRecord B.Builder

instance ( IsRecord    f isRecord
         , ConsToJSON' f isRecord ) => ConsToJSON f where
    consToJSON opts = (unTagged :: Tagged isRecord Value -> Value)
                    . consToJSON' opts (isUnary (undefined :: f a))
    {-# INLINE consToJSON #-}

    consToEncoding opts = (unTagged :: Tagged isRecord B.Builder -> B.Builder)
                          . consToEncoding' opts (isUnary (undefined :: f a))
    {-# INLINE consToEncoding #-}

instance (RecordTo f) => ConsToJSON' f True where
    consToJSON' opts isUn f = let
      vals = toList $ recordToPairs opts f
      in case (unwrapUnaryRecords opts,isUn,vals) of
        (True,True,[(_,val)]) -> Tagged val
        _ -> Tagged $ object vals
    {-# INLINE consToJSON' #-}

    consToEncoding' opts isUn x
      | (True,True) <- (unwrapUnaryRecords opts,isUn) = Tagged $   recordToEncoding opts x
      | otherwise = Tagged $
          B.char7 '{' <>
          recordToEncoding opts x <>
          B.char7 '}'
    {-# INLINE consToEncoding' #-}

instance GToJSON f => ConsToJSON' f False where
    consToJSON' opts _ = Tagged . gToJSON opts
    {-# INLINE consToJSON' #-}
    consToEncoding' opts _ = Tagged . gbuilder opts
    {-# INLINE consToEncoding' #-}

--------------------------------------------------------------------------------

class RecordTo f where
    recordToPairs    :: Options -> f a -> DList Pair
    recordToEncoding :: Options -> f a -> B.Builder

instance (RecordTo a, RecordTo b) => RecordTo (a :*: b) where
    recordToPairs opts (a :*: b) = recordToPairs opts a <>
                                   recordToPairs opts b
    {-# INLINE recordToPairs #-}

    recordToEncoding opts (a :*: b) = recordToEncoding opts a <>
                                      B.char7 ',' <>
                                      recordToEncoding opts b
    {-# INLINE recordToEncoding #-}

instance (Selector s, GToJSON a) => RecordTo (S1 s a) where
    recordToPairs = fieldToPair
    {-# INLINE recordToPairs #-}

    recordToEncoding = fieldToEncoding
    {-# INLINE recordToEncoding #-}

instance (Selector s, ToJSON a) => RecordTo (S1 s (K1 i (Maybe a))) where
    recordToPairs opts (M1 k1) | omitNothingFields opts
                               , K1 Nothing <- k1 = empty
    recordToPairs opts m1 = fieldToPair opts m1
    {-# INLINE recordToPairs #-}

    recordToEncoding opts (M1 k1) | omitNothingFields opts
                                  , K1 Nothing <- k1 = mempty
    recordToEncoding opts m1 = fieldToEncoding opts m1
    {-# INLINE recordToEncoding #-}

fieldToPair :: (Selector s, GToJSON a) => Options -> S1 s a p -> DList Pair
fieldToPair opts m1 = pure ( pack $ fieldLabelModifier opts $ selName m1
                           , gToJSON opts (unM1 m1)
                           )
{-# INLINE fieldToPair #-}

fieldToEncoding :: (Selector s, GToJSON a) => Options -> S1 s a p -> B.Builder
fieldToEncoding opts m1 =
    builder (fieldLabelModifier opts $ selName m1) <>
    B.char7 ':' <>
    gbuilder opts (unM1 m1)
{-# INLINE fieldToEncoding #-}

--------------------------------------------------------------------------------

class WriteProduct f where
    writeProduct :: Options
                 -> VM.MVector s Value
                 -> Int -- ^ index
                 -> Int -- ^ length
                 -> f a
                 -> ST s ()
    encodeProduct :: Options -> f a -> B.Builder

instance ( WriteProduct a
         , WriteProduct b ) => WriteProduct (a :*: b) where
    writeProduct opts mv ix len (a :*: b) = do
      writeProduct opts mv ix  lenL a
      writeProduct opts mv ixR lenR b
        where
          lenL = len `unsafeShiftR` 1
          lenR = len - lenL
          ixR  = ix  + lenL
    {-# INLINE writeProduct #-}

    encodeProduct opts (a :*: b) = encodeProduct opts a <>
                                   B.char7 ',' <>
                                   encodeProduct opts b
    {-# INLINE encodeProduct #-}

instance (GToJSON a) => WriteProduct a where
    writeProduct opts mv ix _ = VM.unsafeWrite mv ix . gToJSON opts
    {-# INLINE writeProduct #-}

    encodeProduct opts = gbuilder opts
    {-# INLINE encodeProduct #-}

--------------------------------------------------------------------------------

class ObjectWithSingleField f where
    objectWithSingleFieldObj :: Options -> f a -> Object
    objectWithSingleFieldEnc :: Options -> f a -> B.Builder

instance ( ObjectWithSingleField a
         , ObjectWithSingleField b ) => ObjectWithSingleField (a :+: b) where
    objectWithSingleFieldObj opts (L1 x) = objectWithSingleFieldObj opts x
    objectWithSingleFieldObj opts (R1 x) = objectWithSingleFieldObj opts x
    {-# INLINE objectWithSingleFieldObj #-}

    objectWithSingleFieldEnc opts (L1 x) = objectWithSingleFieldEnc opts x
    objectWithSingleFieldEnc opts (R1 x) = objectWithSingleFieldEnc opts x
    {-# INLINE objectWithSingleFieldEnc #-}

instance ( GToJSON a, ConsToJSON a
         , Constructor c ) => ObjectWithSingleField (C1 c a) where
    objectWithSingleFieldObj opts = H.singleton typ . gToJSON opts
        where
          typ = pack $ constructorTagModifier opts $
                         conName (undefined :: t c a p)
    {-# INLINE objectWithSingleFieldObj #-}

    objectWithSingleFieldEnc opts v =
      B.char7 '{' <>
      builder (constructorTagModifier opts
               (conName (undefined :: t c a p))) <>
      B.char7 ':' <>
      gbuilder opts v <>
      B.char7 '}'
    {-# INLINE objectWithSingleFieldEnc #-}

gbuilder :: GToJSON f => Options -> f a -> Builder
gbuilder opts = fromEncoding . gToEncoding opts

--------------------------------------------------------------------------------
-- Generic parseJSON

instance (GFromJSON a) => GFromJSON (M1 i c a) where
    -- Meta-information, which is not handled elsewhere, is just added to the
    -- parsed value:
    gParseJSON opts = fmap M1 . gParseJSON opts
    {-# INLINE gParseJSON #-}

instance (FromJSON a) => GFromJSON (K1 i a) where
    -- Constant values are decoded using their FromJSON instance:
    gParseJSON _opts = fmap K1 . parseJSON
    {-# INLINE gParseJSON #-}

instance GFromJSON U1 where
    -- Empty constructors are expected to be encoded as an empty array:
    gParseJSON _opts v
        | isEmptyArray v = pure U1
        | otherwise      = typeMismatch "unit constructor (U1)" v
    {-# INLINE gParseJSON #-}

instance (ConsFromJSON a) => GFromJSON (C1 c a) where
    -- Constructors need to be decoded differently depending on whether they're
    -- a record or not. This distinction is made by consParseJSON:
    gParseJSON opts = fmap M1 . consParseJSON opts
    {-# INLINE gParseJSON #-}

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
    {-# INLINE gParseJSON #-}

instance ( AllNullary (a :+: b) allNullary
         , ParseSum   (a :+: b) allNullary ) => GFromJSON   (a :+: b) where
    -- If all constructors of a sum datatype are nullary and the
    -- 'allNullaryToStringTag' option is set they are expected to be
    -- encoded as strings.  This distinction is made by 'parseSum':
    gParseJSON opts = (unTagged :: Tagged allNullary (Parser ((a :+: b) d)) ->
                                                     (Parser ((a :+: b) d)))
                    . parseSum opts
    {-# INLINE gParseJSON #-}

--------------------------------------------------------------------------------

class ParseSum f allNullary where
    parseSum :: Options -> Value -> Tagged allNullary (Parser (f a))

instance ( SumFromString    (a :+: b)
         , FromPair         (a :+: b)
         , FromTaggedObject (a :+: b) ) => ParseSum (a :+: b) True where
    parseSum opts
        | allNullaryToStringTag opts = Tagged . parseAllNullarySum    opts
        | otherwise                  = Tagged . parseNonAllNullarySum opts
    {-# INLINE parseSum #-}

instance ( FromPair         (a :+: b)
         , FromTaggedObject (a :+: b) ) => ParseSum (a :+: b) False where
    parseSum opts = Tagged . parseNonAllNullarySum opts
    {-# INLINE parseSum #-}

--------------------------------------------------------------------------------

parseAllNullarySum :: SumFromString f => Options -> Value -> Parser (f a)
parseAllNullarySum opts = withText "Text" $ \key ->
                            maybe (notFound $ unpack key) return $
                              parseSumFromString opts key
{-# INLINE parseAllNullarySum #-}

class SumFromString f where
    parseSumFromString :: Options -> Text -> Maybe (f a)

instance (SumFromString a, SumFromString b) => SumFromString (a :+: b) where
    parseSumFromString opts key = (L1 <$> parseSumFromString opts key) <|>
                                  (R1 <$> parseSumFromString opts key)
    {-# INLINE parseSumFromString #-}

instance (Constructor c) => SumFromString (C1 c U1) where
    parseSumFromString opts key | key == name = Just $ M1 U1
                                | otherwise   = Nothing
        where
          name = pack $ constructorTagModifier opts $
                          conName (undefined :: t c U1 p)
    {-# INLINE parseSumFromString #-}

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
{-# INLINE parseNonAllNullarySum #-}

--------------------------------------------------------------------------------

class FromTaggedObject f where
    parseFromTaggedObject :: Options -> String -> Object -> Text
                          -> Maybe (Parser (f a))

instance (FromTaggedObject a, FromTaggedObject b) =>
    FromTaggedObject (a :+: b) where
        parseFromTaggedObject opts contentsFieldName obj tag =
            (fmap L1 <$> parseFromTaggedObject opts contentsFieldName obj tag) <|>
            (fmap R1 <$> parseFromTaggedObject opts contentsFieldName obj tag)
        {-# INLINE parseFromTaggedObject #-}

instance ( FromTaggedObject' f
         , Constructor c ) => FromTaggedObject (C1 c f) where
    parseFromTaggedObject opts contentsFieldName obj tag
        | tag == name = Just $ M1 <$> parseFromTaggedObject'
                                        opts contentsFieldName obj
        | otherwise = Nothing
        where
          name = pack $ constructorTagModifier opts $
                          conName (undefined :: t c f p)
    {-# INLINE parseFromTaggedObject #-}

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
    {-# INLINE parseFromTaggedObject' #-}

instance (FromRecord f) => FromTaggedObject'' f True where
    parseFromTaggedObject'' opts _ = Tagged . parseRecord opts Nothing
    {-# INLINE parseFromTaggedObject'' #-}

instance (GFromJSON f) => FromTaggedObject'' f False where
    parseFromTaggedObject'' opts contentsFieldName = Tagged .
      (gParseJSON opts <=< (.: pack contentsFieldName))
    {-# INLINE parseFromTaggedObject'' #-}

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
    {-# INLINE consParseJSON #-}


instance (FromRecord f) => ConsFromJSON' f True where
    consParseJSON' opts mlab = Tagged . (withObject "record (:*:)"
                                $ parseRecord opts mlab)
    {-# INLINE consParseJSON' #-}

instance (GFromJSON f) => ConsFromJSON' f False where
    consParseJSON' opts _ = Tagged . gParseJSON opts
    {-# INLINE consParseJSON' #-}

--------------------------------------------------------------------------------

class FromRecord f where
    parseRecord :: Options -> (Maybe Text) -- ^ A dummy label
                                           --   (Nothing to use proper label)
                 -> Object -> Parser (f a)

instance (FromRecord a, FromRecord b) => FromRecord (a :*: b) where
    parseRecord opts _ obj = (:*:) <$> parseRecord opts Nothing obj
                                   <*> parseRecord opts Nothing obj
    {-# INLINE parseRecord #-}

instance (Selector s, GFromJSON a) => FromRecord (S1 s a) where
    parseRecord opts (Just lab) = maybe (notFound $ unpack lab)
                      (gParseJSON opts) . H.lookup lab
    parseRecord opts Nothing    = maybe (notFound label)
                      (gParseJSON opts) . H.lookup (pack label)
        where
          label = fieldLabelModifier opts $ selName (undefined :: t s a p)
    {-# INLINE parseRecord #-}

instance (Selector s, FromJSON a) => FromRecord (S1 s (K1 i (Maybe a))) where
    parseRecord _ (Just lab) obj = (M1 . K1) . join <$> obj .:? lab
    parseRecord opts Nothing obj = (M1 . K1) . join <$> obj .:? pack label
        where
          label = fieldLabelModifier opts $
                    selName (undefined :: t s (K1 i (Maybe a)) p)
    {-# INLINE parseRecord #-}

--------------------------------------------------------------------------------

class ProductSize f where
    productSize :: Tagged2 f Int

instance (ProductSize a, ProductSize b) => ProductSize (a :*: b) where
    productSize = Tagged2 $ unTagged2 (productSize :: Tagged2 a Int) +
                            unTagged2 (productSize :: Tagged2 b Int)
    {-# INLINE productSize #-}

instance ProductSize (S1 s a) where
    productSize = Tagged2 1
    {-# INLINE productSize #-}

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
    {-# INLINE parseProduct #-}

instance (GFromJSON a) => FromProduct (S1 s a) where
    parseProduct opts arr ix _ = gParseJSON opts $ V.unsafeIndex arr ix
    {-# INLINE parseProduct #-}

--------------------------------------------------------------------------------

class FromPair f where
    parsePair :: Options -> Pair -> Maybe (Parser (f a))

instance (FromPair a, FromPair b) => FromPair (a :+: b) where
    parsePair opts pair = (fmap L1 <$> parsePair opts pair) <|>
                          (fmap R1 <$> parsePair opts pair)
    {-# INLINE parsePair #-}

instance (Constructor c, GFromJSON a, ConsFromJSON a) => FromPair (C1 c a) where
    parsePair opts (tag, value)
        | tag == tag' = Just $ gParseJSON opts value
        | otherwise   = Nothing
        where
          tag' = pack $ constructorTagModifier opts $
                          conName (undefined :: t c a p)
    {-# INLINE parsePair #-}

--------------------------------------------------------------------------------

class IsRecord (f :: * -> *) isRecord | f -> isRecord
  where
    isUnary :: f a -> Bool
    isUnary = const True

instance (IsRecord f isRecord) => IsRecord (f :*: g) isRecord
  where isUnary = const False
instance IsRecord (M1 S NoSelector f) False
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
