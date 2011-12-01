{-# LANGUAGE CPP, DeriveDataTypeable, Rank2Types #-}

-- |
-- Module:      Data.Aeson.Types.Internal
-- Copyright:   (c) 2011 Bryan O'Sullivan
--              (c) 2011 MailRank, Inc.
-- License:     Apache
-- Maintainer:  Bryan O'Sullivan <bos@serpentine.com>
-- Stability:   experimental
-- Portability: portable
--
-- Types for working with JSON data.

module Data.Aeson.Types.Internal
    (
    -- * Core JSON types
      Value(..)
    , Array
    , emptyArray, isEmptyArray
    , Pair
    , Object
    , emptyObject
    -- * Type conversion
    , Parser
    , Result(..)
    , parse
    , parseEither
    , parseMaybe
    -- * Constructors and accessors
    , object
    ) where

import Control.Applicative
import Control.DeepSeq (NFData(..))
import Control.Monad.State.Strict
import Data.Attoparsec.Char8 (Number(..))
import Data.Hashable (Hashable(..))
import Data.HashMap.Strict (HashMap)
import Data.Monoid (Monoid(..))
import Data.String (IsString(..))
import Data.Text (Text, pack)
import Data.Typeable (Typeable)
import Data.Vector (Vector)
import qualified Data.HashMap.Strict as H
import qualified Data.Vector as V

-- | The result of running a 'Parser'.
data Result a = Error String
              | Success a
                deriving (Eq, Show, Typeable)

instance (NFData a) => NFData (Result a) where
    rnf (Success a) = rnf a
    rnf (Error err) = rnf err

instance Functor Result where
    fmap f (Success a) = Success (f a)
    fmap _ (Error err) = Error err
    {-# INLINE fmap #-}

instance Monad Result where
    return = Success
    {-# INLINE return #-}
    Success a >>= k = k a
    Error err >>= _ = Error err
    {-# INLINE (>>=) #-}

instance Applicative Result where
    pure  = return
    {-# INLINE pure #-}
    (<*>) = ap
    {-# INLINE (<*>) #-}

instance MonadPlus Result where
    mzero = fail "mzero"
    {-# INLINE mzero #-}
    mplus a@(Success _) _ = a
    mplus _ b             = b
    {-# INLINE mplus #-}

instance Alternative Result where
    empty = mzero
    {-# INLINE empty #-}
    (<|>) = mplus
    {-# INLINE (<|>) #-}

instance Monoid (Result a) where
    mempty  = fail "mempty"
    {-# INLINE mempty #-}
    mappend = mplus
    {-# INLINE mappend #-}

-- | Failure continuation.
type Failure f r   = String -> f r
-- | Success continuation.
type Success a f r = a -> f r

-- | A continuation-based parser type.
newtype Parser a = Parser {
      runParser :: forall f r.
                   Failure f r
                -> Success a f r
                -> f r
    }

instance Monad Parser where
    m >>= g = Parser $ \kf ks -> let ks' a = runParser (g a) kf ks
                                 in runParser m kf ks'
    {-# INLINE (>>=) #-}
    return a = Parser $ \_kf ks -> ks a
    {-# INLINE return #-}
    fail msg = Parser $ \kf _ks -> kf msg
    {-# INLINE fail #-}

instance Functor Parser where
    fmap f m = Parser $ \kf ks -> let ks' a = ks (f a)
                                  in runParser m kf ks'
    {-# INLINE fmap #-}

instance Applicative Parser where
    pure  = return
    {-# INLINE pure #-}
    (<*>) = apP
    {-# INLINE (<*>) #-}

instance Alternative Parser where
    empty = fail "empty"
    {-# INLINE empty #-}
    (<|>) = mplus
    {-# INLINE (<|>) #-}

instance MonadPlus Parser where
    mzero = fail "mzero"
    {-# INLINE mzero #-}
    mplus a b = Parser $ \kf ks -> let kf' _ = runParser b kf ks
                                   in runParser a kf' ks
    {-# INLINE mplus #-}

instance Monoid (Parser a) where
    mempty  = fail "mempty"
    {-# INLINE mempty #-}
    mappend = mplus
    {-# INLINE mappend #-}

apP :: Parser (a -> b) -> Parser a -> Parser b
apP d e = do
  b <- d
  a <- e
  return (b a)
{-# INLINE apP #-}

-- | A JSON \"object\" (key\/value map).
type Object = HashMap Text Value

-- | A JSON \"array\" (sequence).
type Array = Vector Value

-- | A JSON value represented as a Haskell value.
data Value = Object !Object
           | Array !Array
           | String !Text
           | Number !Number
           | Bool !Bool
           | Null
             deriving (Eq, Show, Typeable)

instance NFData Value where
    rnf (Object o) = rnf o
    rnf (Array a)  = V.foldl' (\x y -> rnf y `seq` x) () a
    rnf (String s) = rnf s
    rnf (Number n) = case n of I i -> rnf i; D d -> rnf d
    rnf (Bool b)   = rnf b
    rnf Null       = ()

instance IsString Value where
    fromString = String . pack
    {-# INLINE fromString #-}

instance Hashable Value where
    hash (Object o) = H.foldl' hashWithSalt 0 o
    hash (Array a)  = V.foldl' hashWithSalt 1 a
    hash (String s) = 2 `hashWithSalt` s
    hash (Number n) = 3 `hashWithSalt` case n of I i -> hash i; D d -> hash d
    hash (Bool b)   = 4 `hashWithSalt` b
    hash Null       = 5

-- | The empty array.
emptyArray :: Value
emptyArray = Array V.empty

-- | Determines if the 'Value' is an empty 'Array'.
-- Note that: @isEmptyArray 'emptyArray'@.
isEmptyArray :: Value -> Bool
isEmptyArray (Array arr) = V.null arr
isEmptyArray _ = False

-- | The empty object.
emptyObject :: Value
emptyObject = Object H.empty

-- | Run a 'Parser'.
parse :: (a -> Parser b) -> a -> Result b
parse m v = runParser (m v) Error Success
{-# INLINE parse #-}

-- | Run a 'Parser' with a 'Maybe' result type.
parseMaybe :: (a -> Parser b) -> a -> Maybe b
parseMaybe m v = runParser (m v) (const Nothing) Just
{-# INLINE parseMaybe #-}

-- | Run a 'Parser' with an 'Either' result type.
parseEither :: (a -> Parser b) -> a -> Either String b
parseEither m v = runParser (m v) Left Right

-- | A key\/value pair for an 'Object'.
type Pair = (Text, Value)

{-# INLINE parseEither #-}
-- | Create a 'Value' from a list of name\/value 'Pair's.  If duplicate
-- keys arise, earlier keys and their associated values win.
object :: [Pair] -> Value
object = Object . H.fromList
{-# INLINE object #-}
