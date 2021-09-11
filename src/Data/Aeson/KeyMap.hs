{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- An abstract interface for maps from Textual keys to values.

module Data.Aeson.KeyMap (
    -- * Map Type
    KeyMap,

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

newtype KeyMap v = KeyMap { unKeyMap :: HashMap Text v }
  deriving (Eq, Ord, Typeable, Data, Functor)

instance Read v => Read (KeyMap v) where
    readPrec = parens $ prec 10 $ do
      Ident "fromList" <- lexP
      xs <- readPrec
      return (fromList xs)

    readListPrec = readListPrecDefault

instance Show v => Show (KeyMap v) where
    showsPrec d m = showParen (d > 10) $
      showString "fromList " . shows (toAscList m)


#if __GLASGOW_HASKELL__ >= 711
instance Semigroup (KeyMap v) where
   (KeyMap m1) <> (KeyMap m2) = KeyMap (m1 `H.union` m2)
   {-# INLINE (<>) #-}
#endif
instance Monoid (KeyMap v) where
    mempty = empty
    {-# INLINE mempty #-}
#if __GLASGOW_HASKELL__ >= 711
    mappend = (<>)
#else
    mappend (KeyMap m1) (KeyMap m2) = KeyMap (m1 `H.union` m2)
#endif
    {-# INLINE mappend #-}

instance Hashable v => Hashable (KeyMap v) where
    hashWithSalt salt (KeyMap hm) = hashWithSalt salt hm

instance NFData v => NFData (KeyMap v) where
    rnf (KeyMap hm) = rnf hm

instance Foldable KeyMap where
    foldMap f (KeyMap tm) = H.foldMapWithKey (\ _k v -> f v) tm
    {-# INLINE foldMap #-}
    foldr f z (KeyMap tm)  = H.foldr f z tm
    {-# INLINE foldr #-}
    foldl f z (KeyMap tm) = H.foldl f z tm
    {-# INLINE foldl #-}
    foldr' f z (KeyMap tm) = H.foldr' f z tm
    {-# INLINE foldr' #-}
    foldl' f z (KeyMap tm) = H.foldl' f z tm
    {-# INLINE foldl' #-}
#if MIN_VERSION_base(4,8,0)
    null = H.null . unKeyMap
    {-# INLINE null #-}
    length = size
    {-# INLINE length #-}
#endif

instance Traversable KeyMap where
    traverse f = traverseWithKey (const f)
    {-# INLINABLE traverse #-}


instance TH.Lift v => TH.Lift (KeyMap v) where
    lift (KeyMap m) = [| KeyMap (H.fromList . map (first pack) $ m') |]
        where
          m' = map (first unpack) . H.toList $ m

#if MIN_VERSION_template_haskell(2,17,0)
    liftTyped = TH.unsafeCodeCoerce . TH.lift
#elif MIN_VERSION_template_haskell(2,16,0)
    liftTyped = TH.unsafeTExpCoerce . TH.lift
#endif

-- |
-- Construct an empty map.
empty :: KeyMap v
empty = KeyMap H.empty

-- |
-- Return the number of key-value mappings in this map.
size :: KeyMap v -> Int
size = H.size . unKeyMap

-- |
-- Construct a map with a single element.
singleton :: Text -> v -> KeyMap v
singleton k v = KeyMap (H.singleton k v)

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

-- | Reduce this map by applying a binary operator to all
-- elements, using the given starting value (typically the
-- right-identity of the operator).
foldrWithKey :: (Text -> v -> a -> a) -> a -> KeyMap v -> a
foldrWithKey f a = H.foldrWithKey f a . unKeyMap

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

#endif
