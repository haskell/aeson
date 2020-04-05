{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module:      Data.Aeson.Scanner.Internal
-- Copyright:   (c) 2011-2016 Bryan O'Sullivan
--              (c) 2011 MailRank, Inc.
--              (c) 2020 Yuras Shumovich
-- License:     BSD3
-- Maintainer:  Bryan O'Sullivan <bos@serpentine.com>
-- Stability:   experimental
-- Portability: portable
--
-- Efficiently and correctly parse a JSON string. The string must be
-- encoded as UTF-8.

module Data.Aeson.Scanner.Internal
    (
    -- * Lazy scanners
      json, jsonEOF
    , jsonWith
    , jsonLast
    , jsonAccum
    , jsonNoDup
    , value
    , jstring
    , jstring_
    , scientific
    -- * Strict scanners
    , json', jsonEOF'
    , jsonWith'
    , jsonLast'
    , jsonAccum'
    , jsonNoDup'
    , value'
    -- * Helpers
    , decodeWith
    , decodeStrictWith
    , eitherDecodeWith
    , eitherDecodeStrictWith
    -- ** Handling objects with duplicate keys
    , fromListAccum
    , parseListNoDup
    ) where

import Prelude.Compat

import Data.Aeson.Types.Internal
import qualified Data.Aeson.Parser.Internal as Parser
import Data.Bits (testBit)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as Lazy (ByteString)
import Data.Function (fix)
import Data.Functor.Compat (($>))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Scientific (Scientific)
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Word
import Scanner (Scanner)
import qualified Scanner
import qualified Scanner.Attoparsec as Scanner

#define BACKSLASH 92
#define CLOSE_CURLY 125
#define CLOSE_SQUARE 93
#define COMMA 44
#define DOUBLE_QUOTE 34
#define OPEN_CURLY 123
#define OPEN_SQUARE 91
#define C_0 48
#define C_9 57
#define C_A 65
#define C_F 70
#define C_a 97
#define C_f 102
#define C_n 110
#define C_t 116

-- | Parse any JSON value.
--
-- The conversion of a parsed value to a Haskell value is deferred
-- until the Haskell value is needed.  This may improve performance if
-- only a subset of the results of conversions are needed, but at a
-- cost in thunk allocation.
--
-- This function is an alias for 'value'.
--
-- ==== Warning
--
-- If an object contains duplicate keys, only the first one will be kept.
-- For a more flexible alternative, see 'jsonWith'.
json :: Scanner Value
json = value

-- | Parse any JSON value.
--
-- This is a strict version of 'json' which avoids building up thunks
-- during parsing; it performs all conversions immediately.  Prefer
-- this version if most of the JSON data needs to be accessed.
--
-- This function is an alias for 'value''.
--
-- ==== Warning
--
-- If an object contains duplicate keys, only the first one will be kept.
-- For a more flexible alternative, see 'jsonWith''.
json' :: Scanner Value
json' = value'

-- Open recursion: object_, object_', array_, array_' are parameterized by the
-- toplevel Value parser to be called recursively, to keep the parameter
-- mkObject outside of the recursive loop for proper inlining.

object_ :: ([(Text, Value)] -> Either String Object)
        -> Scanner Value -> Scanner Value
object_ mkObject val = {-# SCC "object_" #-} Object
  <$> objectValues mkObject jstring val
{-# INLINE object_ #-}

object_' :: ([(Text, Value)] -> Either String Object)
         -> Scanner Value -> Scanner Value
object_' mkObject val' = {-# SCC "object_'" #-} do
  !vals <- objectValues mkObject jstring' val'
  return (Object vals)
 where
  jstring' = do
    !s <- jstring
    return s
{-# INLINE object_' #-}

objectValues :: ([(Text, Value)] -> Either String Object)
             -> Scanner Text -> Scanner Value -> Scanner (HashMap Text Value)
objectValues mkObject str val = do
  skipSpace
  w <- lookAhead'
  if w == CLOSE_CURLY
    then Scanner.anyWord8 $> HashMap.empty
    else loop []
 where
  -- Why use acc pattern here, you may ask? because 'HashMap.fromList' use
  -- 'unsafeInsert' and it's much faster because it's doing in place update
  -- to the 'HashMap'!
  loop acc = do
    k <- str <* skipSpace <* Scanner.char8 ':'
    v <- val <* skipSpace
    ch <- Scanner.satisfy (\w -> w == COMMA || w == CLOSE_CURLY)
    let acc' = (k, v) : acc
    if ch == COMMA
      then skipSpace >> loop acc'
      else case mkObject acc' of
        Left err -> fail err
        Right obj -> pure obj
{-# INLINE objectValues #-}

array_ :: Scanner Value -> Scanner Value
array_ val = {-# SCC "array_" #-} Array <$> arrayValues val
{-# INLINE array_ #-}

array_' :: Scanner Value -> Scanner Value
array_' val = {-# SCC "array_'" #-} do
  !vals <- arrayValues val
  return (Array vals)
{-# INLINE array_' #-}

arrayValues :: Scanner Value -> Scanner (Vector Value)
arrayValues val = do
  skipSpace
  w <- lookAhead'
  if w == CLOSE_SQUARE
    then Scanner.anyWord8 $> Vector.empty
    else loop [] 1
  where
    loop acc !len = do
      v <- val <* skipSpace
      ch <- Scanner.satisfy (\w -> w == COMMA || w == CLOSE_SQUARE)
      if ch == COMMA
        then skipSpace >> loop (v:acc) (len+1)
        else return (Vector.reverse (Vector.fromListN len (v:acc)))
{-# INLINE arrayValues #-}

-- | Parse any JSON value. Synonym of 'json'.
value :: Scanner Value
value = jsonWith (pure . HashMap.fromList)

-- | Parse any JSON value.
--
-- This parser is parameterized by a function to construct an 'Object'
-- from a raw list of key-value pairs, where duplicates are preserved.
-- The pairs appear in __reverse order__ from the source.
--
-- ==== __Examples__
--
-- 'json' keeps only the first occurence of each key, using 'HashMap.Lazy.fromList'.
--
-- @
-- 'json' = 'jsonWith' ('Right' '.' 'HashMap.fromList')
-- @
--
-- 'jsonLast' keeps the last occurence of each key, using
-- @'HashMap.Lazy.fromListWith' ('const' 'id')@.
--
-- @
-- 'jsonLast' = 'jsonWith' ('Right' '.' 'HashMap.Lazy.fromListWith' ('const' 'id'))
-- @
--
-- 'jsonAccum' keeps wraps all values in arrays to keep duplicates, using
-- 'fromListAccum'.
--
-- @
-- 'jsonAccum' = 'jsonWith' ('Right' . 'fromListAccum')
-- @
--
-- 'jsonNoDup' fails if any object contains duplicate keys, using 'parseListNoDup'.
--
-- @
-- 'jsonNoDup' = 'jsonWith' 'parseListNoDup'
-- @
jsonWith :: ([(Text, Value)] -> Either String Object) -> Scanner Value
jsonWith mkObject = fix $ \value_ -> do
  skipSpace
  w <- lookAhead'
  case w of
    DOUBLE_QUOTE  -> Scanner.anyWord8 *> (String <$> jstring_)
    OPEN_CURLY    -> Scanner.anyWord8 *> object_ mkObject value_
    OPEN_SQUARE   -> Scanner.anyWord8 *> array_ value_
    C_f           -> Scanner.string "false" $> Bool False
    C_t           -> Scanner.string "true" $> Bool True
    C_n           -> Scanner.string "null" $> Null
    _              | w >= 48 && w <= 57 || w == 45
                  -> Number <$> scientific
      | otherwise -> fail "not a valid json value"
{-# INLINE jsonWith #-}

-- | Variant of 'json' which keeps only the last occurence of every key.
jsonLast :: Scanner Value
jsonLast = jsonWith (Right . HashMap.fromListWith (const id))

-- | Variant of 'json' wrapping all object mappings in 'Array' to preserve
-- key-value pairs with the same keys.
jsonAccum :: Scanner Value
jsonAccum = jsonWith (Right . fromListAccum)

-- | Variant of 'json' which fails if any object contains duplicate keys.
jsonNoDup :: Scanner Value
jsonNoDup = jsonWith parseListNoDup

-- | @'fromListAccum' kvs@ is an object mapping keys to arrays containing all
-- associated values from the original list @kvs@.
--
-- >>> fromListAccum [("apple", Bool True), ("apple", Bool False), ("orange", Bool False)]
-- fromList [("apple", [Bool False, Bool True]), ("orange", [Bool False])]
fromListAccum :: [(Text, Value)] -> Object
fromListAccum =
  fmap (Array . Vector.fromList . ($ []))
  . HashMap.fromListWith (.) . (fmap . fmap) (:)

-- | @'fromListNoDup' kvs@ fails if @kvs@ contains duplicate keys.
parseListNoDup :: [(Text, Value)] -> Either String Object
parseListNoDup =
  HashMap.traverseWithKey unwrap . HashMap.fromListWith (\_ _ -> Nothing)
  . (fmap . fmap) Just
  where
    unwrap k Nothing = Left $ "found duplicate key: " ++ show k
    unwrap _ (Just v) = Right v

-- | Strict version of 'value'. Synonym of 'json''.
value' :: Scanner Value
value' = jsonWith' (pure . HashMap.fromList)

-- | Strict version of 'jsonWith'.
jsonWith' :: ([(Text, Value)] -> Either String Object) -> Scanner Value
jsonWith' mkObject = fix $ \value_ -> do
  skipSpace
  w <- lookAhead'
  case w of
    DOUBLE_QUOTE  -> do
                     !s <- Scanner.anyWord8 *> jstring_
                     return (String s)
    OPEN_CURLY    -> Scanner.anyWord8 *> object_' mkObject value_
    OPEN_SQUARE   -> Scanner.anyWord8 *> array_' value_
    C_f           -> Scanner.string "false" $> Bool False
    C_t           -> Scanner.string "true" $> Bool True
    C_n           -> Scanner.string "null" $> Null
    _              | w >= 48 && w <= 57 || w == 45
                  -> do
                     !n <- scientific
                     return (Number n)
      | otherwise -> fail "not a valid json value"
{-# INLINE jsonWith' #-}

-- | Variant of 'json'' which keeps only the last occurence of every key.
jsonLast' :: Scanner Value
jsonLast' = jsonWith' (pure . HashMap.fromListWith (const id))

-- | Variant of 'json'' wrapping all object mappings in 'Array' to preserve
-- key-value pairs with the same keys.
jsonAccum' :: Scanner Value
jsonAccum' = jsonWith' (pure . fromListAccum)

-- | Variant of 'json'' which fails if any object contains duplicate keys.
jsonNoDup' :: Scanner Value
jsonNoDup' = jsonWith' parseListNoDup

-- | Parse a quoted JSON string.
jstring :: Scanner Text
jstring = Scanner.word8 DOUBLE_QUOTE *> jstring_

-- | Parse a string without a leading quote.
jstring_ :: Scanner Text
{-# INLINE jstring_ #-}
jstring_ = do
  s <- Scanner.takeWhile $
    \w -> w /= DOUBLE_QUOTE && w /= BACKSLASH && not (testBit w 7)
  w <- lookAhead'
  case w of
    DOUBLE_QUOTE -> Scanner.anyWord8 $> Text.decodeUtf8 s
    _ -> Scanner.atto $ Parser.jstringSlow s

decodeWith :: Scanner Value -> (Value -> Result a) -> Lazy.ByteString -> Maybe a
decodeWith p to s =
  case Scanner.scanLazy p s of
    Right v -> case to v of
                 Success a -> Just a
                 _         -> Nothing
    _ -> Nothing
{-# INLINE decodeWith #-}

decodeStrictWith :: Scanner Value -> (Value -> Result a) -> ByteString
                 -> Maybe a
decodeStrictWith p to s =
  case either Error to (Scanner.scanOnly p s) of
    Success a -> Just a
    _         -> Nothing
{-# INLINE decodeStrictWith #-}

eitherDecodeWith :: Scanner Value -> (Value -> IResult a) -> Lazy.ByteString
                 -> Either (JSONPath, String) a
eitherDecodeWith p to s =
  case Scanner.scanLazy p s of
    Right v -> case to v of
                          ISuccess a      -> Right a
                          IError path msg -> Left (path, msg)
    Left msg -> Left ([], msg)
{-# INLINE eitherDecodeWith #-}

eitherDecodeStrictWith :: Scanner Value -> (Value -> IResult a) -> ByteString
                       -> Either (JSONPath, String) a
eitherDecodeStrictWith p to s =
  case either (IError []) to (Scanner.scanOnly p s) of
    ISuccess a      -> Right a
    IError path msg -> Left (path, msg)
{-# INLINE eitherDecodeStrictWith #-}

-- $lazy
--
-- The 'json' and 'value' parsers decouple identification from
-- conversion.  Identification occurs immediately (so that an invalid
-- JSON document can be rejected as early as possible), but conversion
-- to a Haskell value is deferred until that value is needed.
--
-- This decoupling can be time-efficient if only a smallish subset of
-- elements in a JSON value need to be inspected, since the cost of
-- conversion is zero for uninspected elements.  The trade off is an
-- increase in memory usage, due to allocation of thunks for values
-- that have not yet been converted.

-- $strict
--
-- The 'json'' and 'value'' parsers combine identification with
-- conversion.  They consume more CPU cycles up front, but have a
-- smaller memory footprint.

-- | Parse a top-level JSON value followed by optional whitespace and
-- end-of-input.  See also: 'json'.
jsonEOF :: Scanner Value
jsonEOF = json <* skipSpace <* endOfInput

-- | Parse a top-level JSON value followed by optional whitespace and
-- end-of-input.  See also: 'json''.
jsonEOF' :: Scanner Value
jsonEOF' = json' <* skipSpace <* endOfInput

-- | The only valid whitespace in a JSON document is space, newline,
-- carriage return, and tab.
skipSpace :: Scanner ()
skipSpace = Scanner.skipWhile
  $ \w -> w == 0x20 || w == 0x0a || w == 0x0d || w == 0x09
{-# INLINE skipSpace #-}

endOfInput :: Scanner ()
endOfInput = do
  w <- Scanner.lookAhead
  case w of
    Nothing -> return ()
    Just _ -> fail "input after data"

-- | Parse a JSON number.
scientific :: Scanner Scientific
scientific = Scanner.atto Parser.scientific

lookAhead' :: Scanner Word8
lookAhead' = Scanner.lookAhead
    >>= maybe (fail "unexpected end of input") return
