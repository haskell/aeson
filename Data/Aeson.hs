-- |
-- Module:      Data.Aeson
-- Copyright:   (c) 2011 Bryan O'Sullivan
--              (c) 2011 MailRank, Inc.
-- License:     Apache
-- Maintainer:  Bryan O'Sullivan <bos@serpentine.com>
-- Stability:   experimental
-- Portability: portable
--
-- Types and functions for working efficiently with JSON data.
--
-- (A note on naming: in Greek mythology, Aeson was the father of Jason.)

module Data.Aeson
    (
    -- * Examples

    -- ** Decoding to an ADT ('Value')
    -- $ex1

    -- ** Decoding to Haskell types
    -- $ex2

    -- ** Decoding a heterogenous object
    -- $ex3

    -- ** Decoding custom data types generically with Typeable
    -- $ex4

    -- ** Pitfalls
    -- $ex5

    -- * Encoding and decoding
      decode
    , decode'
    , eitherDecode
    , eitherDecode'
    , encode
    -- * Core JSON types
    , Value(..)
    , Array
    , Object
    -- * Convenience types
    , DotNetTime(..)
    -- * Type conversion
    , FromJSON(..)
    , Result(..)
    , fromJSON
    , ToJSON(..)
    -- * Inspecting @'Value's@
    , withObject
    , withText
    , withArray
    , withNumber
    , withBool
    -- * Constructors and accessors
    , (.=)
    , (.:)
    , (.:?)
    , (.!=)
    , object
    -- * Parsing
    , json
    , json'
    ) where

import Data.Aeson.Encode (encode)
import Data.Aeson.Parser.Internal (decodeWith, eitherDecodeWith, json, json')
import Data.Aeson.Types
import qualified Data.ByteString.Lazy as L

-- | Efficiently deserialize a JSON value from a lazy 'L.ByteString'.
-- If this fails due to incomplete or invalid input, 'Nothing' is
-- returned.
--
-- This function parses immediately, but defers conversion.  See
-- 'json' for details.
decode :: (FromJSON a) => L.ByteString -> Maybe a
decode = decodeWith json fromJSON
{-# INLINE decode #-}

-- | Efficiently deserialize a JSON value from a lazy 'L.ByteString'.
-- If this fails due to incomplete or invalid input, 'Nothing' is
-- returned.
--
-- This function parses and performs conversion immediately.  See
-- 'json'' for details.
decode' :: (FromJSON a) => L.ByteString -> Maybe a
decode' = decodeWith json' fromJSON
{-# INLINE decode' #-}

-- | Like 'decode' but returns an error message when decoding fails.
eitherDecode :: (FromJSON a) => L.ByteString -> Either String a
eitherDecode = eitherDecodeWith json fromJSON
{-# INLINE eitherDecode #-}

-- | Like 'decode'' but returns an error message when decoding fails.
eitherDecode' :: (FromJSON a) => L.ByteString -> Either String a
eitherDecode' = eitherDecodeWith json' fromJSON
{-# INLINE eitherDecode' #-}

-- $ex1
--
-- To parse JSON into something useful, everything goes through the
-- 'decode' function, which is polymorphic on the 'FromJSON'
-- class. For representing arbitrary JSON AST there is a 'Value' type,
-- which is an instance of 'FromJSON'. For example:
--
-- >>> decode "{\"foo\":123}" :: Maybe Value
-- Just (Object (fromList [("foo",Number 123)]))
-- >>> decode "{\"foo\":[\"abc\",\"def\"]}" :: Maybe Value
-- Just (Object (fromList [("foo",Array (fromList [String "abc",String "def"]))]))
--
-- To run these examples, you need to enable @OverloadedStrings@ (in
-- GHCi you can write @:set -XOverloadedStrings@) so that you can use
-- string literals for non-'String' types. We're using (the lazy
-- version of) 'Data.ByteString.Lazy.ByteString', which requires at
-- least version 0.9.0.4 of the bytestring package to provide the
-- 'Data.String.IsString' instance. You probably have something newer
-- than this installed.

-- $ex2
--
-- Any instance of 'FromJSON' can be specified (but see the PITFALLS section):
--
-- >>> decode "[1,2,3]" :: Maybe [Int]
-- Just [1,2,3]
--
-- Alternatively, there are instances for standard data types, so you
-- can use them directly. For example, use the 'Data.Map.Map' type to
-- get a map of 'Int's.
--
-- >>> :m + Data.Map
-- >>> decode "{\"foo\":1,\"bar\":2}" :: Maybe (Map String Int)
-- Just (fromList [("bar",2),("foo",1)])

-- $ex3
--
-- The above approach with maps of course will not work for
-- heterogenous objects, so there are a couple of approaches available
-- to you.
--
-- The 'Object' type contains JSON objects:
--
-- >>> decode "{\"name\":\"Dave\",\"age\":2}" :: Maybe Object
-- Just (fromList) [("name",String "Dave"),("age",Number 2)]
--
-- And you extract values from it with a parser using 'parse',
-- 'parseEither' or, in this example, 'parseMaybe':
--
-- > dave =
-- >   do result <- decode "{\"name\":\"Dave\",\"age\":2}"
-- >      flip parseMaybe result $ \obj -> do
-- >        age <- obj .: "age"
-- >        name <- obj .: "name"
-- >        return (name ++ ": " ++ show (age*2))
--
-- >>> dave
-- Just "Dave: 4"
--
-- Considering that any type that implements 'FromJSON' can be used
-- here, this is quite a powerful way to parse JSON. See the
-- documentation in 'FromJSON' for how to implement this class for
-- your own data types.
--
-- The downside is that you have to write the parser yourself, the
-- upside is that you have complete control over the way the JSON is
-- parsed.

-- $ex4
--
-- If you don't want such control and would prefer the JSON be parsed
-- to your own data types automatically according to some reasonably
-- sensible isomorphic implementation, you can use the generic parser
-- based on 'Data.Typeable.Typeable' and 'Data.Data.Data'. Switch to
-- the 'Data.Aeson.Generic' module, and you can do the following:
--
-- >>> decode "[1]" :: Maybe [Int]
-- Just [1]
-- >>> :m + Data.Typeable Data.Data
-- >>> :set -XDeriveDataTypeable
-- >>> data Person = Person { personName :: String, personAge :: Int } deriving (Data,Typeable,Show)
-- >>> encode Person { personName = "Chris", personAge = 123 }
-- "{\"personAge\":123,\"personName\":\"Chris\"}"
-- >>> decode "{\"personAge\":123,\"personName\":\"Chris\"}" :: Maybe Person
-- Just (Person { personName = "Chris", personAge = 123 })
--
-- Be aware that the encoding might not be what you expect:
--
-- >>> data Foo = Foo Int Int deriving (Data,Typeable,Show)
-- >>> encode (Foo 1 2)
-- "[1,2]"
--
-- So it's better to treat the 'Data.Aeson.Generic.decode' and
-- 'Data.Aeson.Generic.encode' functions as an isomorphism, but do not
-- rely or care about the actual intermediate representation.

-- $ex5
--
-- Note that the JSON standard only allows arrays or objects of things
-- at the top-level, so calling decode on a simple type will not work:
--
-- >>> decode "1" :: Maybe Int
-- Nothing
-- >>> decode "1" :: Maybe String
-- Nothing
--
-- So stick to objects (e.g. maps in Haskell) or arrays (lists in Haskell):
--
-- >>> decode "[1,2,3]" :: Maybe [Int]
-- Just [1,2,3]
--
-- Likewise, for encoding to JSON you can encode anything that's an
-- instance of 'ToJSON', which does include simple types. So beware
-- that this aspect of the API is not isomorphic:
--
-- >>> encode [1,2,3]
-- "[1,2,3]"
-- >>> decode (encode [1]) :: Maybe [Int]
-- Just [1]
-- >>> encode 1
-- "1"
-- >>> decode (encode (1 :: Int)) :: Maybe Int
-- Nothing
--
-- Alternatively see 'Data.Aeson.Parser.value' to parse non-toplevel
-- JSON values.
