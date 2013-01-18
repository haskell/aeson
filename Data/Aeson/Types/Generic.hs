{-# LANGUAGE DefaultSignatures, EmptyDataDecls, FlexibleInstances,
    FunctionalDependencies, KindSignatures, OverlappingInstances,
    ScopedTypeVariables, TypeOperators, UndecidableInstances, ViewPatterns #-}
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
import Data.DList (DList, toList)
import Data.Monoid (mappend)
import Data.Text (pack, unpack)
import GHC.Generics
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
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

instance (GObject a, GObject b) => GToJSON (a :+: b) where
    gToJSON opts (L1 x) = Object $ gObject opts x
    gToJSON opts (R1 x) = Object $ gObject opts x
    {-# INLINE gToJSON #-}

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

class GObject f where
    gObject :: Options -> f a -> Object

instance (GObject a, GObject b) => GObject (a :+: b) where
    gObject opts (L1 x) = gObject opts x
    gObject opts (R1 x) = gObject opts x
    {-# INLINE gObject #-}

instance (Constructor c, GToJSON a, ConsToJSON a) => GObject (C1 c a) where
    gObject opts = H.singleton (pack $ constructorNameModifier opts
                                     $ conName (undefined :: t c a p))
                 . gToJSON opts
    {-# INLINE gObject #-}

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

instance (GFromSum a, GFromSum b) => GFromJSON (a :+: b) where
    gParseJSON opts (Object (H.toList -> [keyVal@(key, _)])) =
        case gParseSum opts keyVal of
          Nothing -> notFound $ unpack key
          Just p  -> p
    gParseJSON _opts v = typeMismatch "sum (:+:)" v
    {-# INLINE gParseJSON #-}

notFound :: String -> Parser a
notFound key = fail $ "The key \"" ++ key ++ "\" was not found"
{-# INLINE notFound #-}

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
                      . H.lookup (T.pack key)
        where
          key = fieldNameModifier opts $ selName (undefined :: t s a p)
    {-# INLINE gParseRecord #-}

--------------------------------------------------------------------------------

class ProductSize f where
    productSize :: Tagged2 f Int

newtype Tagged2 (s :: * -> *) b = Tagged2 {unTagged2 :: b}

instance (ProductSize a, ProductSize b) => ProductSize (a :*: b) where
    productSize = Tagged2 $ unTagged2 (productSize :: Tagged2 a Int) +
                            unTagged2 (productSize :: Tagged2 b Int)

instance ProductSize (S1 s a) where
    productSize = Tagged2 1

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

class GFromSum f where
    gParseSum :: Options -> Pair -> Maybe (Parser (f a))

instance (GFromSum a, GFromSum b) => GFromSum (a :+: b) where
    gParseSum opts keyVal = (fmap L1 <$> gParseSum opts keyVal) <|>
                            (fmap R1 <$> gParseSum opts keyVal)
    {-# INLINE gParseSum #-}

instance (Constructor c, GFromJSON a, ConsFromJSON a) => GFromSum (C1 c a) where
    gParseSum opts (key, value)
        | key == pack ( constructorNameModifier opts
                      $ conName (undefined :: t c a p)
                      )
                    = Just $ gParseJSON opts value
        | otherwise = Nothing
    {-# INLINE gParseSum #-}

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
