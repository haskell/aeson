{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}

#if __GLASGOW_HASKELL__ >= 800
-- a) THQ works on cross-compilers and unregisterised GHCs
-- b) may make compilation faster as no dynamic loading is ever needed (not sure about this)
-- c) removes one hindrance to have code inferred as SafeHaskell safe
{-# LANGUAGE TemplateHaskellQuotes #-}
#else
{-# LANGUAGE TemplateHaskell #-}
#endif

-- |
-- An abstract interface for maps from JSON keys to values.

module Data.Aeson.KeyMap (
    -- * Map Type
    KeyMap,

    -- * Query
    null,
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
    union,
    unionWith,
    unionWithKey,
    intersection,
    intersectionWith,
    intersectionWithKey,
    alignWith,
    alignWithKey,

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
    map,
    mapKeyVal,
    traverse,
    traverseWithKey,

    -- * Folds
    foldr,
    foldr',
    foldl,
    foldl',
    foldMapWithKey,
    foldrWithKey,

    -- * Conversions
    keys,

    -- * Filter
    filter,
    filterWithKey,
    mapMaybe,
    mapMaybeWithKey,
) where

-- Import stuff from Prelude explicitly
import Prelude (Eq(..), Ord((>)), Int, Bool(..), Maybe(..))
import Prelude ((.), ($), fst)
import Prelude (Functor(fmap), Monad(..))
import Prelude (Show, showsPrec, showParen, shows, showString)

import Control.Applicative (Applicative)
import Control.DeepSeq (NFData(..))
import Data.Data (Data)
import Data.Hashable (Hashable(..))
import Data.Monoid (Monoid(mempty, mappend))
import Data.Semigroup (Semigroup((<>)))
import Data.Text (Text, unpack, pack)
import Data.These (These (..))
import Data.Typeable (Typeable)
import Text.Read (Read (..), Lexeme(..), readListPrecDefault, prec, lexP, parens)

import qualified Data.List as L
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import qualified Language.Haskell.TH.Syntax as TH
import qualified Data.Foldable.WithIndex    as WI (FoldableWithIndex (..))
import qualified Data.Functor.WithIndex     as WI (FunctorWithIndex (..))
import qualified Data.Traversable.WithIndex as WI (TraversableWithIndex (..))
import qualified Data.Semialign as SA
import qualified Data.Semialign.Indexed as SAI

#ifdef MIN_VERSION_witherable
import qualified Witherable as W
#endif

#if 1
import Data.HashMap.Strict (HashMap)
import Data.List (sortBy)
import Data.Ord (comparing)
import Control.Arrow (first)

import qualified Data.HashMap.Strict as H

-- | A map from JSON key type 'Key' to 'v'.
newtype KeyMap v = KeyMap { unKeyMap :: HashMap Text v }
  deriving (Eq, Ord, Typeable, Data, Functor)

-- | Construct an empty map.
empty :: KeyMap v
empty = KeyMap H.empty

-- | Is the map empty?
null :: KeyMap v -> Bool
null = H.null . unKeyMap

-- | Return the number of key-value mappings in this map.
size :: KeyMap v -> Int
size = H.size . unKeyMap

-- | Construct a map with a single element.
singleton :: Text -> v -> KeyMap v
singleton k v = KeyMap (H.singleton k v)

-- | Is the key a member of the map?
member :: Text -> KeyMap a -> Bool
member t (KeyMap m) = H.member t m

-- | Return the value to which the specified key is mapped,
-- or Nothing if this map contains no mapping for the key.
lookup :: Text -> KeyMap v -> Maybe v
lookup t tm = H.lookup t (unKeyMap tm)

-- | Associate the specified value with the specified key
-- in this map. If this map previously contained a mapping
-- for the key, the old value is replaced.
insert :: Text -> v -> KeyMap v -> KeyMap v
insert k v tm = KeyMap (H.insert k v (unKeyMap tm))

-- | Map a function over all values in the map.
map :: (a -> b) -> KeyMap a -> KeyMap b
map = fmap

-- | Map a function over all values in the map.
mapWithKey :: (Text -> a -> b) -> KeyMap a -> KeyMap b
mapWithKey f (KeyMap m) = KeyMap (H.mapWithKey f m)

foldMapWithKey :: Monoid m => (Text -> a -> m) -> KeyMap a -> m
foldMapWithKey f (KeyMap m) = H.foldMapWithKey f m

foldr :: (a -> b -> b) -> b -> KeyMap a -> b
foldr f z (KeyMap m) = H.foldr f z m

foldr' :: (a -> b -> b) -> b -> KeyMap a -> b
foldr' f z (KeyMap m) = H.foldr' f z m

foldl :: (b -> a -> b) -> b -> KeyMap a -> b
foldl f z (KeyMap m) = H.foldl f z m

foldl' :: (b -> a -> b) -> b -> KeyMap a -> b
foldl' f z (KeyMap m) = H.foldl' f z m

-- | Reduce this map by applying a binary operator to all
-- elements, using the given starting value (typically the
-- right-identity of the operator).
foldrWithKey :: (Text -> v -> a -> a) -> a -> KeyMap v -> a
foldrWithKey f a = H.foldrWithKey f a . unKeyMap

-- | Perform an Applicative action for each key-value pair
-- in a 'KeyMap' and produce a 'KeyMap' of all the results.
traverse :: Applicative f => (v1 -> f v2) -> KeyMap v1 -> f (KeyMap v2)
traverse f = fmap KeyMap . T.traverse f . unKeyMap

-- | Perform an Applicative action for each key-value pair
-- in a 'KeyMap' and produce a 'KeyMap' of all the results.
traverseWithKey :: Applicative f => (Text -> v1 -> f v2) -> KeyMap v1 -> f (KeyMap v2)
traverseWithKey f = fmap KeyMap . H.traverseWithKey f  . unKeyMap

-- | Construct a map from a list of elements. Uses the
-- provided function, f, to merge duplicate entries with
-- (f newVal oldVal).
fromListWith :: (v -> v -> v) ->  [(Text, v)] -> KeyMap v
fromListWith op = KeyMap . H.fromListWith op

-- |  Construct a map with the supplied mappings. If the
-- list contains duplicate mappings, the later mappings take
-- precedence.
fromList :: [(Text, v)] -> KeyMap v
fromList = KeyMap . H.fromList

-- | Return a list of this map's elements.
--
-- The order is not stable. Use 'toAscList' for stable ordering.
toList :: KeyMap v -> [(Text, v)]
toList = H.toList . unKeyMap

-- | Return a list of this map's elements in ascending order
-- based of the textual key.
toAscList :: KeyMap v -> [(Text, v)]
toAscList = sortBy (comparing fst) . toList

-- | Difference of two maps. Return elements of the first
-- map not existing in the second.
difference :: KeyMap v -> KeyMap v' -> KeyMap v
difference tm1 tm2 = KeyMap (H.difference (unKeyMap tm1) (unKeyMap tm2))

-- The (left-biased) union of two maps. It prefers the first map when duplicate
-- keys are encountered, i.e. ('union' == 'unionWith' 'const').
union :: KeyMap v -> KeyMap v -> KeyMap v
union (KeyMap x) (KeyMap y) = KeyMap (H.union x y)

-- | The union with a combining function.
unionWith :: (v -> v -> v) -> KeyMap v -> KeyMap v -> KeyMap v
unionWith f (KeyMap x) (KeyMap y) = KeyMap (H.unionWith f x y)

-- | The union with a combining function.
unionWithKey :: (Text -> v -> v -> v) -> KeyMap v -> KeyMap v -> KeyMap v
unionWithKey f (KeyMap x) (KeyMap y) = KeyMap (H.unionWithKey f x y)

-- | The (left-biased) intersection of two maps (based on keys).
intersection :: KeyMap a -> KeyMap b -> KeyMap a
intersection (KeyMap x) (KeyMap y) = KeyMap (H.intersection x y)

-- | The intersection with a combining function.
intersectionWith :: (a -> b -> c) -> KeyMap a -> KeyMap b -> KeyMap c
intersectionWith f (KeyMap x) (KeyMap y) = KeyMap (H.intersectionWith f x y)

-- | The intersection with a combining function.
intersectionWithKey :: (Text -> a -> b -> c) -> KeyMap a -> KeyMap b -> KeyMap c
intersectionWithKey f (KeyMap x) (KeyMap y) = KeyMap (H.intersectionWithKey f x y)

-- | Return a list of this map's keys.
keys :: KeyMap v -> [Text]
keys = H.keys . unKeyMap

-- | Convert a 'KeyMap' to a 'HashMap'.
toHashMap :: KeyMap v -> HashMap Text v
toHashMap = unKeyMap

-- | Convert a 'HashMap' to a 'KeyMap'.
fromHashMap :: HashMap Text v -> KeyMap v
fromHashMap = KeyMap

-- | Transform the keys and values of a 'KeyMap'.
mapKeyVal :: (Text -> Text) -> (v1 -> v2)
          -> KeyMap v1 -> KeyMap v2
mapKeyVal fk kv = foldrWithKey (\k v -> insert (fk k) (kv v)) empty
{-# INLINE mapKeyVal #-}

-- | Filter all keys/values that satisfy some predicate.
filter :: (v -> Bool) -> KeyMap v -> KeyMap v
filter f (KeyMap m) = KeyMap (H.filter f m)

-- | Filter all keys/values that satisfy some predicate.
filterWithKey :: (Text -> v -> Bool) -> KeyMap v -> KeyMap v
filterWithKey f (KeyMap m) = KeyMap (H.filterWithKey f m)

-- | Map values and collect the Just results.
mapMaybe :: (a -> Maybe b) -> KeyMap a -> KeyMap b
mapMaybe f (KeyMap m) = KeyMap (H.mapMaybe f m)

-- | Map values and collect the Just results.
mapMaybeWithKey :: (Text -> v -> Maybe u) -> KeyMap v -> KeyMap u
mapMaybeWithKey f (KeyMap m) = KeyMap (H.mapMaybeWithKey f m)

#endif

-------------------------------------------------------------------------------
-- combinators using existing abstractions
-------------------------------------------------------------------------------

-- | Generalized union with combining function.
alignWith :: (These a b -> c) -> KeyMap a -> KeyMap b -> KeyMap c
alignWith f (KeyMap x) (KeyMap y) = KeyMap (SA.alignWith f x y)

-- | Generalized union with combining function.
alignWithKey :: (Text -> These a b -> c) -> KeyMap a -> KeyMap b -> KeyMap c
alignWithKey f (KeyMap x) (KeyMap y) = KeyMap (SAI.ialignWith f x y)

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

-- This are defined using concrete combinators above.

instance Read v => Read (KeyMap v) where
    readPrec = parens $ prec 10 $ do
      Ident "fromList" <- lexP
      xs <- readPrec
      return (fromList xs)

    readListPrec = readListPrecDefault

instance Show v => Show (KeyMap v) where
    showsPrec d m = showParen (d > 10) $
      showString "fromList " . shows (toAscList m)

instance F.Foldable KeyMap where
    foldMap f = foldMapWithKey (\ _k v -> f v)
    {-# INLINE foldMap #-}
    foldr = foldr
    foldr' = foldr'
    foldl = foldl
    foldl' = foldl'
#if MIN_VERSION_base(4,8,0)
    null = null
    length = size
#endif

instance T.Traversable KeyMap where
    traverse = traverse

instance Semigroup (KeyMap v) where
    (<>) = union

instance Monoid (KeyMap v) where
    mempty = empty
#if __GLASGOW_HASKELL__ >= 711
    mappend = (<>)
#else
    mappend = union
#endif

-------------------------------------------------------------------------------
-- template-haskell
-------------------------------------------------------------------------------

instance TH.Lift v => TH.Lift (KeyMap v) where
    lift m = [| fromList (L.map (first pack) m') |]
        where
          m' = L.map (first unpack) . toList $ m

#if MIN_VERSION_template_haskell(2,17,0)
    liftTyped = TH.unsafeCodeCoerce . TH.lift
#elif MIN_VERSION_template_haskell(2,16,0)
    liftTyped = TH.unsafeTExpCoerce . TH.lift
#endif

-------------------------------------------------------------------------------
-- hashable
-------------------------------------------------------------------------------

instance Hashable v => Hashable (KeyMap v) where
    hashWithSalt salt (KeyMap hm) = hashWithSalt salt hm

-------------------------------------------------------------------------------
-- deepseq
-------------------------------------------------------------------------------

instance NFData v => NFData (KeyMap v) where
    rnf (KeyMap hm) = rnf hm

-------------------------------------------------------------------------------
-- indexed-traversable
-------------------------------------------------------------------------------

instance WI.FunctorWithIndex Text KeyMap where
    imap = mapWithKey

instance WI.FoldableWithIndex Text KeyMap where
    ifoldr   = foldrWithKey

instance WI.TraversableWithIndex Text KeyMap where
    itraverse = traverseWithKey

-------------------------------------------------------------------------------
-- semialign
-------------------------------------------------------------------------------

instance SA.Zip KeyMap where
    zipWith = intersectionWith

instance SAI.ZipWithIndex Text KeyMap where
    izipWith = intersectionWithKey

instance SA.Semialign KeyMap where
    alignWith = alignWith

instance SAI.SemialignWithIndex Text KeyMap where
    ialignWith = alignWithKey

instance SA.Align KeyMap where
    nil = empty

-------------------------------------------------------------------------------
-- witherable
-------------------------------------------------------------------------------

#ifdef MIN_VERSION_witherable
instance W.Filterable KeyMap where
    filter = filter
    mapMaybe = mapMaybe

instance W.Witherable KeyMap where

instance W.FilterableWithIndex Text KeyMap where
    ifilter = filterWithKey
    imapMaybe = mapMaybeWithKey

instance W.WitherableWithIndex Text KeyMap where
#endif
