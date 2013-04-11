{-# LANGUAGE DefaultSignatures, EmptyDataDecls, FlexibleInstances,
    FunctionalDependencies, KindSignatures, OverlappingInstances,
    ScopedTypeVariables, TypeOperators, UndecidableInstances,
    ViewPatterns, NamedFieldPuns, FlexibleContexts, PatternGuards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module:      Data.Aeson.Types.Generic
-- Copyright:   (c) 2012 Bryan O'Sullivan
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
import Control.Monad.ST (ST)
import Data.Aeson.Types.Class
import Data.Aeson.Types.Internal
import Data.Bits (shiftR)
import Data.DList (DList, toList, empty)
import Data.Monoid (mappend)
import Data.Text (Text, pack, unpack)
import GHC.Generics
import qualified Data.HashMap.Strict as H
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

--------------------------------------------------------------------------------
-- Generic toJSON

instance (GToJSON a) => GToJSON (M1 i c a) where
    gToJSON opts = gToJSON opts . unM1
    {-# INLINE gToJSON #-}

instance (ToJSON a) => GToJSON (K1 i a) where
    gToJSON _opts = toJSON . unK1
    {-# INLINE gToJSON #-}

instance GToJSON U1 where
    gToJSON _opts _ = emptyArray
    {-# INLINE gToJSON #-}

instance (ConsToJSON a) => GToJSON (C1 c a) where
    gToJSON opts = consToJSON opts . unM1
    {-# INLINE gToJSON #-}

instance ( GProductToValues a, GProductToValues b
         , ProductSize      a, ProductSize      b) => GToJSON (a :*: b) where
    gToJSON opts p =
        Array $ V.create $ do
          mv <- VM.unsafeNew lenProduct
          gProductToValues opts mv 0 lenProduct p
          return mv
        where
          lenProduct = unTagged2 (productSize :: Tagged2 (a :*: b) Int)
    {-# INLINE gToJSON #-}

instance (AllNullary (a :+: b) c, GSumToJSON' c (a :+: b)) => GToJSON (a :+: b) where
    gToJSON = unTagged (gSumToJSON' :: Tagged c (Options -> (a :+: b) d -> Value))
    {-# INLINE gToJSON #-}

class GSumToJSON' b f where
    gSumToJSON' :: Tagged b (Options -> f a -> Value)

instance ( GSumToString           f
         , GExtractSum            f
         , GObjectWithType        f
         , GObjectWithSingleField f
         ) => GSumToJSON' True f where
    gSumToJSON' = Tagged $ \opts x ->
                    if nullaryToString opts
                    then gSumToString  opts x
                    else sumToJSON     opts x
    {-# INLINE gSumToJSON' #-}

instance (GExtractSum f, GObjectWithType f, GObjectWithSingleField f) => GSumToJSON' False f where
    gSumToJSON' = Tagged sumToJSON
    {-# INLINE gSumToJSON' #-}

sumToJSON :: (GExtractSum f, GObjectWithType f, GObjectWithSingleField f) => Options -> f a -> Value
sumToJSON opts x =
    case sumEncoding opts of
      TwoElemArray ->
          Array $ V.create $ do
            let (typ, val) = gExtractSum x opts
            mv <- VM.unsafeNew 2
            VM.unsafeWrite mv 0 typ
            VM.unsafeWrite mv 1 val
            return mv
      ObjectWithType{typeFieldName,valueFieldName} ->
          object $ gObjectWithType opts typeFieldName valueFieldName x
      ObjectWithSingleField ->
          Object $ gObjectWithSingleField opts x
{-# INLINE sumToJSON #-}

--------------------------------------------------------------------------------

class GObjectWithType f where
    gObjectWithType :: Options -> String -> String -> f a -> [Pair]

instance (GObjectWithType a, GObjectWithType b) => GObjectWithType (a :+: b) where
    gObjectWithType     opts typeFieldName valueFieldName (L1 x) =
        gObjectWithType opts typeFieldName valueFieldName     x
    gObjectWithType     opts typeFieldName valueFieldName (R1 x) =
        gObjectWithType opts typeFieldName valueFieldName     x
    {-# INLINE gObjectWithType #-}

instance (IsRecord a b, Constructor c, GObjectWithType' b a) =>
    GObjectWithType (C1 c a) where
    gObjectWithType opts typeFieldName valueFieldName x =
        (pack typeFieldName .= constructorNameModifier opts
                                 (conName (undefined :: t c a p))) :
        unTagged (gObjectWithType' opts valueFieldName (unM1 x) :: Tagged b [Pair])
    {-# INLINE gObjectWithType #-}

class GObjectWithType' b f where
    gObjectWithType' :: Options -> String -> f a -> Tagged b [Pair]

instance (GRecordToPairs f) => GObjectWithType' True f where
    gObjectWithType' opts _ x = Tagged $ toList $ gRecordToPairs opts x
    {-# INLINE gObjectWithType' #-}

instance (GToJSON f) => GObjectWithType' False f where
    gObjectWithType' opts valueFieldName x =
        Tagged [pack valueFieldName .= gToJSON opts x]
    {-# INLINE gObjectWithType' #-}

--------------------------------------------------------------------------------

class GSumToString f where
    gSumToString :: Options -> f a -> Value

instance (GSumToString a, GSumToString b) => GSumToString (a :+: b) where
    gSumToString opts (L1 x) = gSumToString opts x
    gSumToString opts (R1 x) = gSumToString opts x
    {-# INLINE gSumToString #-}

instance (Constructor c, GToJSON a, ConsToJSON a) => GSumToString (C1 c a) where
    gSumToString opts _ = String $ pack $ constructorNameModifier opts
                                        $ conName (undefined :: t c a p)
    {-# INLINE gSumToString #-}

--------------------------------------------------------------------------------

class GExtractSum f where
    gExtractSum :: f a -> Options -> (Value, Value)

instance (GExtractSum a, GExtractSum b) => GExtractSum (a :+: b) where
    gExtractSum (L1 x) = gExtractSum x
    gExtractSum (R1 x) = gExtractSum x
    {-# INLINE gExtractSum #-}

instance (Constructor c, GToJSON a, ConsToJSON a) => GExtractSum (C1 c a) where
    gExtractSum x opts = ( String $ pack $ constructorNameModifier opts
                                         $ conName (undefined :: t c a p)
                         , gToJSON opts x
                         )
    {-# INLINE gExtractSum #-}

--------------------------------------------------------------------------------

class ConsToJSON    f where consToJSON  ::           Options -> f a -> Value
class ConsToJSON' b f where consToJSON' :: Tagged b (Options -> f a -> Value)

newtype Tagged s b = Tagged {unTagged :: b}

instance (IsRecord f b, ConsToJSON' b f) => ConsToJSON f where
    consToJSON = unTagged (consToJSON' :: Tagged b (Options -> f a -> Value))
    {-# INLINE consToJSON #-}

instance (GRecordToPairs f) => ConsToJSON' True f where
    consToJSON' = Tagged (\opts -> object . toList . gRecordToPairs opts)
    {-# INLINE consToJSON' #-}

instance GToJSON f => ConsToJSON' False f where
    consToJSON' = Tagged gToJSON
    {-# INLINE consToJSON' #-}

--------------------------------------------------------------------------------

class GRecordToPairs f where
    gRecordToPairs :: Options -> f a -> DList Pair

instance (GRecordToPairs a, GRecordToPairs b) => GRecordToPairs (a :*: b) where
    gRecordToPairs opts (a :*: b) = gRecordToPairs opts a `mappend`
                                    gRecordToPairs opts b
    {-# INLINE gRecordToPairs #-}

instance (Selector s, GToJSON a) => GRecordToPairs (S1 s a) where
    gRecordToPairs opts m1 = pure ( pack $ fieldNameModifier opts $ selName m1
                                  , gToJSON opts (unM1 m1)
                                  )
    {-# INLINE gRecordToPairs #-}

instance (Selector s, ToJSON a) => GRecordToPairs (S1 s (K1 i (Maybe a))) where
    gRecordToPairs opts (M1 k1) | omitNothingFields opts
                                , K1 Nothing <- k1 = empty
    gRecordToPairs opts m1 = pure ( pack $ fieldNameModifier opts $ selName m1
                                  , gToJSON opts (unM1 m1)
                                  )
    {-# INLINE gRecordToPairs #-}

--------------------------------------------------------------------------------

class GProductToValues f where
    gProductToValues :: Options
                     -> VM.MVector s Value
                     -> Int -- ^ index
                     -> Int -- ^ length
                     -> f a
                     -> ST s ()

instance (GProductToValues a, GProductToValues b) => GProductToValues (a :*: b) where
    gProductToValues opts mv ix len (a :*: b) = do
      gProductToValues opts mv ix  lenL a
      gProductToValues opts mv ixR lenR b
        where
          lenL = len `shiftR` 1
          ixR  = ix + lenL
          lenR = len - lenL
    {-# INLINE gProductToValues #-}

instance (GToJSON a) => GProductToValues a where
    gProductToValues opts mv ix _ = VM.unsafeWrite mv ix . gToJSON opts
    {-# INLINE gProductToValues #-}

--------------------------------------------------------------------------------

class GObjectWithSingleField f where
    gObjectWithSingleField :: Options -> f a -> Object

instance (GObjectWithSingleField a, GObjectWithSingleField b) =>
    GObjectWithSingleField (a :+: b) where
    gObjectWithSingleField opts (L1 x) = gObjectWithSingleField opts x
    gObjectWithSingleField opts (R1 x) = gObjectWithSingleField opts x
    {-# INLINE gObjectWithSingleField #-}

instance (Constructor c, GToJSON a, ConsToJSON a) =>
    GObjectWithSingleField (C1 c a) where
    gObjectWithSingleField opts x =
        H.singleton ( pack $ constructorNameModifier opts
                    $ conName (undefined :: t c a p)
                    ) $ gToJSON opts x
    {-# INLINE gObjectWithSingleField #-}

--------------------------------------------------------------------------------
-- Generic parseJSON

instance (GFromJSON a) => GFromJSON (M1 i c a) where
    gParseJSON opts = fmap M1 . gParseJSON opts
    {-# INLINE gParseJSON #-}

instance (FromJSON a) => GFromJSON (K1 i a) where
    gParseJSON _opts = fmap K1 . parseJSON
    {-# INLINE gParseJSON #-}

instance GFromJSON U1 where
    gParseJSON _opts v
        | isEmptyArray v = pure U1
        | otherwise      = typeMismatch "unit constructor (U1)" v
    {-# INLINE gParseJSON #-}

instance (ConsFromJSON a) => GFromJSON (C1 c a) where
    gParseJSON opts = fmap M1 . consParseJSON opts
    {-# INLINE gParseJSON #-}

instance ( GFromProduct a, GFromProduct b
         , ProductSize a, ProductSize b) => GFromJSON (a :*: b) where
    gParseJSON opts (Array arr)
        | lenArray == lenProduct = gParseProduct opts arr 0 lenProduct
        | otherwise =
            fail $ "When expecting a product of " ++ show lenProduct ++
                   " values, encountered an Array of " ++ show lenArray ++
                   " elements instead"
        where
          lenArray = V.length arr
          lenProduct = unTagged2 (productSize :: Tagged2 (a :*: b) Int)

    gParseJSON _opts v = typeMismatch "product (:*:)" v
    {-# INLINE gParseJSON #-}

instance (AllNullary (a :+: b) c, GParseSum' c (a :+: b)) => GFromJSON (a :+: b) where
    gParseJSON = unTagged (gParseSum' :: Tagged c (Options -> Value -> Parser ((a :+: b) d)))
    {-# INLINE gParseJSON #-}

class GParseSum' b f where
    gParseSum' :: Tagged b (Options -> Value -> Parser (f a))

instance (GSumFromString (a :+: b), GFromPair (a :+: b), GFromObjectWithType (a :+: b)) =>
    GParseSum' True (a :+: b) where
    gParseSum' = Tagged $ \opts v ->
                   if nullaryToString      opts
                   then parseSumFromString opts v
                   else parseSum           opts v
    {-# INLINE gParseSum' #-}

instance (GFromPair (a :+: b), GFromObjectWithType (a :+: b)) => GParseSum' False (a :+: b) where
    gParseSum' = Tagged parseSum
    {-# INLINE gParseSum' #-}

--------------------------------------------------------------------------------

parseSumFromString :: GSumFromString f => Options -> Value -> Parser (f a)
parseSumFromString opts (String key) = case gParseSumFromString opts key of
                                         Nothing -> notFound $ unpack key
                                         Just x  -> return x
parseSumFromString _ v = typeMismatch "String" v
{-# INLINE parseSumFromString #-}

class GSumFromString f where
    gParseSumFromString :: Options -> Text -> Maybe (f a)

instance (GSumFromString a, GSumFromString b) => GSumFromString (a :+: b) where
    gParseSumFromString opts key = (L1 <$> gParseSumFromString opts key) <|>
                                   (R1 <$> gParseSumFromString opts key)
    {-# INLINE gParseSumFromString #-}

instance (Constructor c) => GSumFromString (C1 c U1) where
    gParseSumFromString opts key | key == name = Just $ M1 U1
                                 | otherwise   = Nothing
        where
          name = pack $ constructorNameModifier opts $ conName (undefined :: t c U1 p)
    {-# INLINE gParseSumFromString #-}

--------------------------------------------------------------------------------

parseSum :: (GFromPair (a :+: b), GFromObjectWithType (a :+: b)) =>
            Options -> Value -> Parser ((a :+: b) c)
parseSum opts v =
    case sumEncoding opts of
      TwoElemArray -> parseTwoElemArray opts v
      ObjectWithType{typeFieldName,valueFieldName} ->
          parseFromObjectWithType opts typeFieldName valueFieldName v
      ObjectWithSingleField -> parseFromObjectWithSingleField opts v
{-# INLINE parseSum #-}

parseFromObjectWithSingleField :: (GFromPair (a :+: b)) => Options -> Value -> Parser ((a :+: b) c)
parseFromObjectWithSingleField opts (Object (H.toList -> [keyVal@(key, _)])) =
    case gParsePair opts keyVal of
      Nothing -> notFound $ unpack key
      Just p  -> p
parseFromObjectWithSingleField _opts v = typeMismatch "Object with single field" v
{-# INLINE parseFromObjectWithSingleField #-}

notFound :: String -> Parser a
notFound key = fail $ "The key \"" ++ key ++ "\" was not found"
{-# INLINE notFound #-}

--------------------------------------------------------------------------------

parseTwoElemArray :: GFromPair f => Options -> Value -> Parser (f a)
parseTwoElemArray opts (Array arr)
    | V.length arr == 2
    , String key <- V.unsafeIndex arr 0 =
        case gParsePair opts (key, V.unsafeIndex arr 1) of
          Nothing -> notFound $ unpack key
          Just p  -> p
parseTwoElemArray _ v = typeMismatch "Array with a key and value element" v
{-# INLINE parseTwoElemArray #-}

--------------------------------------------------------------------------------

parseFromObjectWithType :: (GFromObjectWithType f)
                        => Options -> String -> String -> Value -> Parser (f a)
parseFromObjectWithType opts typeFieldName valueFieldName (Object obj) = do
  key <- obj .: pack typeFieldName
  case gParseFromObjectWithType opts valueFieldName obj key of
    Nothing -> notFound $ unpack key
    Just p  -> p
parseFromObjectWithType _ _ _ v = typeMismatch "Object" v
{-# INLINE parseFromObjectWithType #-}

class GFromObjectWithType f where
    gParseFromObjectWithType :: Options -> String -> Object -> Text -> Maybe (Parser (f a))

instance (GFromObjectWithType a, GFromObjectWithType b) =>
    GFromObjectWithType (a :+: b) where
        gParseFromObjectWithType opts valueFieldName obj key =
            (fmap L1 <$> gParseFromObjectWithType opts valueFieldName obj key) <|>
            (fmap R1 <$> gParseFromObjectWithType opts valueFieldName obj key)
        {-# INLINE gParseFromObjectWithType #-}

instance (GFromObjectWithType' f, Constructor c) => GFromObjectWithType (C1 c f) where
    gParseFromObjectWithType opts valueFieldName obj key
        | key == name = Just $ M1 <$> gParseFromObjectWithType' opts valueFieldName obj
        | otherwise = Nothing
        where
          name = pack $ constructorNameModifier opts $ conName (undefined :: t c f p)
    {-# INLINE gParseFromObjectWithType #-}

class GFromObjectWithType' f where
    gParseFromObjectWithType' :: Options -> String -> Object -> Parser (f a)

instance (IsRecord f b, GFromObjectWithType'' b f) => GFromObjectWithType' f where
    gParseFromObjectWithType' =
        unTagged (gParseFromObjectWithType'' ::
                      Tagged b (Options -> String -> Object -> Parser (f a)))
    {-# INLINE gParseFromObjectWithType' #-}

class GFromObjectWithType'' b f where
    gParseFromObjectWithType'' :: Tagged b (Options -> String -> Object -> Parser (f a))

instance (GFromRecord f) => GFromObjectWithType'' True f where
    gParseFromObjectWithType'' = Tagged $ \opts _ obj -> gParseRecord opts obj
    {-# INLINE gParseFromObjectWithType'' #-}

instance (GFromJSON f) => GFromObjectWithType'' False f where
    gParseFromObjectWithType'' = Tagged $ \opts valueFieldName obj ->
      gParseJSON opts =<< (obj .: pack valueFieldName)
    {-# INLINE gParseFromObjectWithType'' #-}

--------------------------------------------------------------------------------

class ConsFromJSON    f where
    consParseJSON  ::           Options -> Value -> Parser (f a)
class ConsFromJSON' b f where
    consParseJSON' :: Tagged b (Options -> Value -> Parser (f a))

instance (IsRecord f b, ConsFromJSON' b f) => ConsFromJSON f where
    consParseJSON =
        unTagged (consParseJSON' :: Tagged b (Options -> Value -> Parser (f a)))
    {-# INLINE consParseJSON #-}

instance (GFromRecord f) => ConsFromJSON' True f where
    consParseJSON' = Tagged parseRecord
        where
          parseRecord  opts (Object obj) = gParseRecord opts obj
          parseRecord _opts v = typeMismatch "record (:*:)" v
    {-# INLINE consParseJSON' #-}

instance (GFromJSON f) => ConsFromJSON' False f where
    consParseJSON' = Tagged gParseJSON
    {-# INLINE consParseJSON' #-}

--------------------------------------------------------------------------------

class GFromRecord f where
    gParseRecord :: Options -> Object -> Parser (f a)

instance (GFromRecord a, GFromRecord b) => GFromRecord (a :*: b) where
    gParseRecord opts obj = (:*:) <$> gParseRecord opts obj
                                  <*> gParseRecord opts obj
    {-# INLINE gParseRecord #-}

instance (Selector s, GFromJSON a) => GFromRecord (S1 s a) where
    gParseRecord opts = maybe (notFound key) (gParseJSON opts)
                      . H.lookup (pack key)
        where
          key = fieldNameModifier opts $ selName (undefined :: t s a p)
    {-# INLINE gParseRecord #-}

instance (Selector s, FromJSON a) => GFromRecord (S1 s (K1 i (Maybe a))) where
    gParseRecord opts obj = (M1 . K1) <$> obj .:? pack key
        where
          key = fieldNameModifier opts $
                  selName (undefined :: t s (K1 i (Maybe a)) p)
    {-# INLINE gParseRecord #-}

--------------------------------------------------------------------------------

class ProductSize f where
    productSize :: Tagged2 f Int

newtype Tagged2 (s :: * -> *) b = Tagged2 {unTagged2 :: b}

instance (ProductSize a, ProductSize b) => ProductSize (a :*: b) where
    productSize = Tagged2 $ unTagged2 (productSize :: Tagged2 a Int) +
                            unTagged2 (productSize :: Tagged2 b Int)
    {-# INLINE productSize #-}

instance ProductSize (S1 s a) where
    productSize = Tagged2 1
    {-# INLINE productSize #-}

--------------------------------------------------------------------------------

class GFromProduct f where
    gParseProduct :: Options -> Array -> Int -> Int -> Parser (f a)

instance (GFromProduct a, GFromProduct b) => GFromProduct (a :*: b) where
    gParseProduct opts arr ix len =
        (:*:) <$> gParseProduct opts arr ix  lenL
              <*> gParseProduct opts arr ixR lenR
        where
          lenL = len `shiftR` 1
          ixR  = ix + lenL
          lenR = len - lenL
    {-# INLINE gParseProduct #-}

instance (GFromJSON a) => GFromProduct (S1 s a) where
    gParseProduct opts arr ix _ = gParseJSON opts $ V.unsafeIndex arr ix
    {-# INLINE gParseProduct #-}

--------------------------------------------------------------------------------

class GFromPair f where
    gParsePair :: Options -> Pair -> Maybe (Parser (f a))

instance (GFromPair a, GFromPair b) => GFromPair (a :+: b) where
    gParsePair opts keyVal = (fmap L1 <$> gParsePair opts keyVal) <|>
                             (fmap R1 <$> gParsePair opts keyVal)
    {-# INLINE gParsePair #-}

instance (Constructor c, GFromJSON a, ConsFromJSON a) => GFromPair (C1 c a) where
    gParsePair opts (key, value)
        | key == name = Just $ gParseJSON opts value
        | otherwise   = Nothing
        where
          name = pack $ constructorNameModifier opts $ conName (undefined :: t c a p)
    {-# INLINE gParsePair #-}

--------------------------------------------------------------------------------

class IsRecord (f :: * -> *) b | f -> b

data True
data False

instance (IsRecord f b) => IsRecord (f :*: g) b
instance IsRecord (M1 S NoSelector f) False
instance (IsRecord f b) => IsRecord (M1 S c f) b
instance IsRecord (K1 i c) True
instance IsRecord U1 False

--------------------------------------------------------------------------------

class AllNullary (f :: * -> *) b | f -> b

instance (AllNullary a b1, AllNullary c b2, And b1 b2 b3) =>
    AllNullary (a :+: c) b3
instance AllNullary a b => AllNullary (M1 i c a) b
instance AllNullary (a :*: c) False
instance AllNullary (K1 i c) False
instance AllNullary U1 True

class And b1 b2 b3 | b1 b2 -> b3

instance And True True    True

instance And False False  False
instance And False True   False
instance And True  False  False

--------------------------------------------------------------------------------
