{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- An abstract interface for maps from Textual keys to values.

module Data.Aeson.TextMap (
    -- * Map Type
    TextMap,

    -- * Query
    lookup,
    size,
    member,

    -- * Construction
    empty,
    singleton,

    -- ** Insertion
    insert,

    -- * Combine
    difference,

    -- * Lists
    fromList,
    fromListWith,
    toList,
    toAscList,

    -- * HashMaps
    fromHashMap,
    toHashMap,

    -- * Traversal
    -- ** Map
    mapKeyVal,
    traverseWithKey,

    -- * Folds
    foldrWithKey,

    -- * Conversions
    keys,
) where

#if 1
import Control.DeepSeq (NFData(..))
import Data.Data (Data)
import Data.Hashable (Hashable(..))
import Data.HashMap.Strict (HashMap)
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Text (Text, unpack, pack)
import Data.Typeable (Typeable)
import Prelude hiding (lookup)
import Control.Arrow (first)
import Data.Foldable hiding (toList)
import Text.Read
#if __GLASGOW_HASKELL__ < 710
import Data.Monoid (Monoid(mempty, mappend))
import Data.Traversable (Traversable(..))
import Control.Applicative (Applicative)
#endif
#if __GLASGOW_HASKELL__ >= 711
import Data.Semigroup (Semigroup((<>)))
#endif

import qualified Data.HashMap.Strict as H
import qualified Language.Haskell.TH.Syntax as TH

newtype TextMap v = TextMap { unTextMap :: HashMap Text v }
  deriving (Eq, Ord, Typeable, Data, Functor)

instance Read v => Read (TextMap v) where
    readPrec = parens $ prec 10 $ do
      Ident "fromList" <- lexP
      xs <- readPrec
      return (fromList xs)

    readListPrec = readListPrecDefault

instance Show v => Show (TextMap v) where
    showsPrec d m = showParen (d > 10) $
      showString "fromList " . shows (toAscList m)


#if __GLASGOW_HASKELL__ >= 711
instance Semigroup (TextMap v) where
   (TextMap m1) <> (TextMap m2) = TextMap (m1 `H.union` m2)
   {-# INLINE (<>) #-}
#endif
instance Monoid (TextMap v) where
    mempty = empty
    {-# INLINE mempty #-}
#if __GLASGOW_HASKELL__ >= 711
    mappend = (<>)
#else
    mappend (TextMap m1) (TextMap m2) = TextMap (m1 `H.union` m2)
#endif
    {-# INLINE mappend #-}

instance Hashable v => Hashable (TextMap v) where
    hashWithSalt salt (TextMap hm) = hashWithSalt salt hm

instance NFData v => NFData (TextMap v) where
    rnf (TextMap hm) = rnf hm

instance Foldable TextMap where
    foldMap f (TextMap tm) = H.foldMapWithKey (\ _k v -> f v) tm
    {-# INLINE foldMap #-}
    foldr f z (TextMap tm)  = H.foldr f z tm
    {-# INLINE foldr #-}
    foldl f z (TextMap tm) = H.foldl f z tm
    {-# INLINE foldl #-}
    foldr' f z (TextMap tm) = H.foldr' f z tm
    {-# INLINE foldr' #-}
    foldl' f z (TextMap tm) = H.foldl' f z tm
    {-# INLINE foldl' #-}
#if MIN_VERSION_base(4,8,0)
    null = H.null . unTextMap
    {-# INLINE null #-}
    length = size
    {-# INLINE length #-}
#endif

instance Traversable TextMap where
    traverse f = traverseWithKey (const f)
    {-# INLINABLE traverse #-}


instance TH.Lift v => TH.Lift (TextMap v) where
    lift (TextMap m) = [| TextMap (H.fromList . map (first pack) $ m') |]
        where
          m' = map (first unpack) . H.toList $ m

#if MIN_VERSION_template_haskell(2,17,0)
    liftTyped = TH.unsafeCodeCoerce . TH.lift
#elif MIN_VERSION_template_haskell(2,16,0)
    liftTyped = TH.unsafeTExpCoerce . TH.lift
#endif

-- |
-- Construct an empty map.
empty :: TextMap v
empty = TextMap H.empty

-- |
-- Return the number of key-value mappings in this map.
size :: TextMap v -> Int
size = H.size . unTextMap

-- |
-- Construct a map with a single element.
singleton :: Text -> v -> TextMap v
singleton k v = TextMap (H.singleton k v)

member :: Text -> TextMap a -> Bool
member t (TextMap m) = H.member t m

-- | Return the value to which the specified key is mapped,
-- or Nothing if this map contains no mapping for the key.
lookup :: Text -> TextMap v -> Maybe v
lookup t tm = H.lookup t (unTextMap tm)

-- | Associate the specified value with the specified key
-- in this map. If this map previously contained a mapping
-- for the key, the old value is replaced.
insert :: Text -> v -> TextMap v -> TextMap v
insert k v tm = TextMap (H.insert k v (unTextMap tm))

-- | Reduce this map by applying a binary operator to all
-- elements, using the given starting value (typically the
-- right-identity of the operator).
foldrWithKey :: (Text -> v -> a -> a) -> a -> TextMap v -> a
foldrWithKey f a = H.foldrWithKey f a . unTextMap

-- | Perform an Applicative action for each key-value pair
-- in a 'TextMap' and produce a 'TextMap' of all the results.
traverseWithKey :: Applicative f => (Text -> v1 -> f v2) -> TextMap v1 -> f (TextMap v2)
traverseWithKey f = fmap TextMap . H.traverseWithKey f  . unTextMap

-- | Construct a map from a list of elements. Uses the
-- provided function, f, to merge duplicate entries with
-- (f newVal oldVal).
fromListWith :: (v -> v -> v) ->  [(Text, v)] -> TextMap v
fromListWith op = TextMap . H.fromListWith op

-- |  Construct a map with the supplied mappings. If the
-- list contains duplicate mappings, the later mappings take
-- precedence.
fromList :: [(Text, v)] -> TextMap v
fromList = TextMap . H.fromList

-- | Return a list of this map's elements.
toList :: TextMap v -> [(Text, v)]
toList = H.toList . unTextMap

-- | Return a list of this map's elements in ascending order
-- based of the textual key.
toAscList :: TextMap v -> [(Text, v)]
toAscList = sortBy (comparing fst) . toList

-- | Difference of two maps. Return elements of the first
-- map not existing in the second.
difference :: TextMap v -> TextMap v' -> TextMap v
difference tm1 tm2 = TextMap (H.difference (unTextMap tm1) (unTextMap tm2))

-- | Return a list of this map's keys.
keys :: TextMap v -> [Text]
keys = H.keys . unTextMap

-- | Convert a 'TextMap' to a 'HashMap'.
toHashMap :: TextMap v -> HashMap Text v
toHashMap = unTextMap

-- | Convert a 'HashMap' to a 'TextMap'.
fromHashMap :: HashMap Text v -> TextMap v
fromHashMap = TextMap

-- | Transform the keys and values of a 'TextMap'.
mapKeyVal :: (Text -> Text) -> (v1 -> v2)
          -> TextMap v1 -> TextMap v2
mapKeyVal fk kv = foldrWithKey (\k v -> insert (fk k) (kv v)) empty
{-# INLINE mapKeyVal #-}

#endif
