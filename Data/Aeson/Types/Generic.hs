{-# LANGUAGE DefaultSignatures, EmptyDataDecls, FlexibleContexts,
    FlexibleInstances, FunctionalDependencies, IncoherentInstances,
    KindSignatures, OverlappingInstances, ScopedTypeVariables, TypeOperators,
    UndecidableInstances, ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module:      Data.Aeson.Types.Generic
-- Copyright:   (c) 2011 MailRank, Inc.
-- License:     Apache
-- Maintainer:  Bryan O'Sullivan <bos@mailrank.com>
-- Stability:   experimental
-- Portability: portable
--
-- Types for working with JSON data.

module Data.Aeson.Types.Generic ( ) where

import Control.Applicative
import Control.Monad.State.Strict
import Data.Aeson.Types.Class
import Data.Aeson.Types.Internal
import Data.Text (pack, unpack)
import Data.Vector ((!?))
import GHC.Generics
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Vector as V

--------------------------------------------------------------------------------
-- Generic toJSON

instance (GToJSON a) => GToJSON (M1 i c a) where
    gToJSON = gToJSON . unM1
    {-# INLINE gToJSON #-}

instance (ToJSON a) => GToJSON (K1 i a) where
    gToJSON = toJSON . unK1
    {-# INLINE gToJSON #-}

instance GToJSON U1 where
    gToJSON _ = emptyArray
    {-# INLINE gToJSON #-}

instance (ConsToJSON a) => GToJSON (C1 c a) where
    gToJSON = consToJSON . unM1
    {-# INLINE gToJSON #-}

instance (GProductToValues a, GProductToValues b) => GToJSON (a :*: b) where
    gToJSON = toJSON . toList . gProductToValues
    {-# INLINE gToJSON #-}

instance (GObject a, GObject b) => GToJSON (a :+: b) where
    gToJSON (L1 x) = Object $ gObject x
    gToJSON (R1 x) = Object $ gObject x
    {-# INLINE gToJSON #-}

--------------------------------------------------------------------------------

class ConsToJSON    f where consToJSON  ::           f a -> Value
class ConsToJSON' b f where consToJSON' :: Tagged b (f a -> Value)

instance (IsRecord f b, ConsToJSON' b f) => ConsToJSON f where
    consToJSON = unTagged (consToJSON' :: Tagged b (f a -> Value))
    {-# INLINE consToJSON #-}

instance (GRecordToPairs f) => ConsToJSON' True f where
    consToJSON' = Tagged (object . toList . gRecordToPairs)
    {-# INLINE consToJSON' #-}

instance GToJSON f => ConsToJSON' False f where
    consToJSON' = Tagged gToJSON
    {-# INLINE consToJSON' #-}

--------------------------------------------------------------------------------

class GRecordToPairs f where
    gRecordToPairs :: f a -> DList Pair

instance (GRecordToPairs a, GRecordToPairs b) => GRecordToPairs (a :*: b) where
    gRecordToPairs (a :*: b) = gRecordToPairs a `append` gRecordToPairs b
    {-# INLINE gRecordToPairs #-}

instance (Selector s, GToJSON a) => GRecordToPairs (S1 s a) where
    gRecordToPairs m1 = singleton (pack (selName m1), gToJSON (unM1 m1))
    {-# INLINE gRecordToPairs #-}

--------------------------------------------------------------------------------

class GProductToValues f where
    gProductToValues :: f a -> DList Value

instance (GProductToValues a, GProductToValues b) => GProductToValues (a :*: b) where
    gProductToValues (a :*: b) = gProductToValues a `append` gProductToValues b
    {-# INLINE gProductToValues #-}

instance (GToJSON a) => GProductToValues a where
    gProductToValues = singleton . gToJSON
    {-# INLINE gProductToValues #-}

--------------------------------------------------------------------------------

class GObject f where
    gObject :: f a -> Object

instance (GObject a, GObject b) => GObject (a :+: b) where
    gObject (L1 x) = gObject x
    gObject (R1 x) = gObject x
    {-# INLINE gObject #-}

instance (Constructor c, GToJSON a, ConsToJSON a) => GObject (C1 c a) where
    gObject m1 = M.singleton (pack (conName m1)) (gToJSON m1)
    {-# INLINE gObject #-}

--------------------------------------------------------------------------------
-- Generic parseJSON

instance (GFromJSON a) => GFromJSON (M1 i c a) where
    gParseJSON = fmap M1 . gParseJSON
    {-# INLINE gParseJSON #-}

instance (FromJSON a) => GFromJSON (K1 i a) where
    gParseJSON = fmap K1 . parseJSON
    {-# INLINE gParseJSON #-}

instance GFromJSON U1 where
    gParseJSON v
        | isEmptyArray v = pure U1
        | otherwise      = typeMismatch "unit constructor (U1)" v
    {-# INLINE gParseJSON #-}

instance (ConsFromJSON a) => GFromJSON (C1 c a) where
    gParseJSON = fmap M1 . consParseJSON
    {-# INLINE gParseJSON #-}

instance (GFromProduct a, GFromProduct b) => GFromJSON (a :*: b) where
    gParseJSON (Array arr) = gParseProduct arr
    gParseJSON v = typeMismatch "product (:*:)" v
    {-# INLINE gParseJSON #-}

instance (GFromSum a, GFromSum b) => GFromJSON (a :+: b) where
    gParseJSON (Object (M.toList -> [keyVal])) = gParseSum keyVal
    gParseJSON v = typeMismatch "sum (:+:)" v
    {-# INLINE gParseJSON #-}

--------------------------------------------------------------------------------

class ConsFromJSON    f where consParseJSON  ::           Value -> Parser (f a)
class ConsFromJSON' b f where consParseJSON' :: Tagged b (Value -> Parser (f a))

instance (IsRecord f b, ConsFromJSON' b f) => ConsFromJSON f where
    consParseJSON = unTagged (consParseJSON' :: Tagged b (Value -> Parser (f a)))
    {-# INLINE consParseJSON #-}

instance (GFromRecord f) => ConsFromJSON' True f where
    consParseJSON' = Tagged parseRecord
        where
          parseRecord (Object obj) = gParseRecord obj
          parseRecord v = typeMismatch "record (:*:)" v
    {-# INLINE consParseJSON' #-}

instance (GFromJSON f) => ConsFromJSON' False f where
    consParseJSON' = Tagged gParseJSON
    {-# INLINE consParseJSON' #-}

--------------------------------------------------------------------------------

class GFromRecord f where
    gParseRecord :: Object -> Parser (f a)

instance (GFromRecord a, GFromRecord b) => GFromRecord (a :*: b) where
    gParseRecord obj = (:*:) <$> gParseRecord obj <*> gParseRecord obj
    {-# INLINE gParseRecord #-}

instance (Selector s, GFromJSON a) => GFromRecord (S1 s a) where
    gParseRecord obj = case M.lookup (T.pack key) obj of
                         Nothing -> notFound key
                         Just v  -> gParseJSON v
        where
          key = selName (undefined :: t s a p)
    {-# INLINE gParseRecord #-}

--------------------------------------------------------------------------------

class GFromProduct f where
    gParseProduct :: Array -> Parser (f a)

instance (GFromProduct a, GFromProduct b) => GFromProduct (a :*: b) where
    gParseProduct arr = (:*:) <$> gParseProduct arrL <*> gParseProduct arrR
        where
          (arrL, arrR) = V.splitAt (V.length arr `div` 2) arr
    {-# INLINE gParseProduct #-}

instance (GFromJSON a) => GFromProduct a where
    gParseProduct ((!? 0) -> Just v) = gParseJSON v
    gParseProduct _ = fail "Array to small"
    {-# INLINE gParseProduct #-}

--------------------------------------------------------------------------------

class GFromSum f where
    gParseSum :: Pair -> Parser (f a)

instance (GFromSum a, GFromSum b) => GFromSum (a :+: b) where
    gParseSum keyVal = (L1 <$> gParseSum keyVal) <|> (R1 <$> gParseSum keyVal)
    {-# INLINE gParseSum #-}

instance (Constructor c, GFromJSON a, ConsFromJSON a) => GFromSum (C1 c a) where
    gParseSum (key, value)
        | key == pack (conName (undefined :: t c a p)) = gParseJSON value
        | otherwise = notFound $ unpack key
    {-# INLINE gParseSum #-}

notFound :: String -> Parser a
notFound key = fail $ "The key \"" ++ key ++ "\" was not found"

--------------------------------------------------------------------------------

newtype Tagged s b = Tagged {unTagged :: b}

data True
data False

class IsRecord (f :: * -> *) b | f -> b

instance (IsRecord f b) => IsRecord (f :*: g) b
instance IsRecord (M1 S NoSelector f) False
instance (IsRecord f b) => IsRecord (M1 S c f) b
instance IsRecord (K1 i c) True
instance IsRecord U1 False

--------------------------------------------------------------------------------

type DList a = [a] -> [a]

toList :: DList a -> [a]
toList = ($ [])
{-# INLINE toList #-}

singleton :: a -> DList a
singleton = (:)
{-# INLINE singleton #-}

append :: DList a -> DList a -> DList a
append = (.)
{-# INLINE append #-}

--------------------------------------------------------------------------------
