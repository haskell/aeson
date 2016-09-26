{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
#if __GLASGOW_HASKELL__ >= 800
-- a) THQ works on cross-compilers and unregisterised GHCs
-- b) may make compilation faster as no dynamic loading is ever needed (not sure about this)
-- c) removes one hindrance to have code inferred as SafeHaskell safe
{-# LANGUAGE TemplateHaskellQuotes #-}
#else
{-# LANGUAGE TemplateHaskell #-}
#endif

-- |
-- Module:      Data.Aeson.Types.Internal
-- Copyright:   (c) 2011-2016 Bryan O'Sullivan
--              (c) 2011 MailRank, Inc.
-- License:     BSD3
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
    , IResult(..)
    , JSONPathElement(..)
    , JSONPath
    , iparse
    , parse
    , parseEither
    , parseMaybe
    , modifyFailure
    , formatError
    , (<?>)
    -- * Constructors and accessors
    , object

    -- * Generic and TH encoding configuration
    , Options(..)
    , SumEncoding(..)
    , defaultOptions
    , defaultTaggedObject

    -- * Used for changing CamelCase names into something else.
    , camelTo
    , camelTo2

    -- * Other types
    , DotNetTime(..)
    ) where

import Prelude ()
import Prelude.Compat

import Control.Applicative (Alternative(..))
import Control.Arrow (first)
import Control.DeepSeq (NFData(..))
import Control.Monad (MonadPlus(..), ap)
import Data.Char (isLower, isUpper, toLower, isAlpha, isAlphaNum)
import Data.Data (Data)
import Data.Foldable (foldl')
import Data.HashMap.Strict (HashMap)
import Data.Hashable (Hashable(..))
import Data.Scientific (Scientific)
import Data.Semigroup (Semigroup((<>)))
import Data.String (IsString(..))
import Data.Text (Text, pack, unpack)
import Data.Time (UTCTime)
import Data.Time.Format (FormatTime)
import Data.Typeable (Typeable)
import Data.Vector (Vector)
import qualified Control.Monad.Fail as Fail
import qualified Data.HashMap.Strict as H
import qualified Data.Scientific as S
import qualified Data.Vector as V
import qualified Language.Haskell.TH.Syntax as TH

#if !MIN_VERSION_unordered_containers(0,2,6)
import Data.List (sort)
#endif

-- | Elements of a JSON path used to describe the location of an
-- error.
data JSONPathElement = Key Text
                       -- ^ JSON path element of a key into an object,
                       -- \"object.key\".
                     | Index {-# UNPACK #-} !Int
                       -- ^ JSON path element of an index into an
                       -- array, \"array[index]\".
                       deriving (Eq, Show, Typeable)
type JSONPath = [JSONPathElement]

-- | The internal result of running a 'Parser'.
data IResult a = IError JSONPath String
               | ISuccess a
               deriving (Eq, Show, Typeable)

-- | The result of running a 'Parser'.
data Result a = Error String
              | Success a
                deriving (Eq, Show, Typeable)

instance NFData JSONPathElement where
  rnf (Key t)   = rnf t
  rnf (Index i) = rnf i

instance (NFData a) => NFData (IResult a) where
    rnf (ISuccess a)      = rnf a
    rnf (IError path err) = rnf path `seq` rnf err

instance (NFData a) => NFData (Result a) where
    rnf (Success a) = rnf a
    rnf (Error err) = rnf err

instance Functor IResult where
    fmap f (ISuccess a)      = ISuccess (f a)
    fmap _ (IError path err) = IError path err
    {-# INLINE fmap #-}

instance Functor Result where
    fmap f (Success a) = Success (f a)
    fmap _ (Error err) = Error err
    {-# INLINE fmap #-}

instance Monad IResult where
    return = pure
    {-# INLINE return #-}

    ISuccess a      >>= k = k a
    IError path err >>= _ = IError path err
    {-# INLINE (>>=) #-}

    fail = Fail.fail
    {-# INLINE fail #-}

instance Fail.MonadFail IResult where
    fail err = IError [] err
    {-# INLINE fail #-}

instance Monad Result where
    return = pure
    {-# INLINE return #-}

    Success a >>= k = k a
    Error err >>= _ = Error err
    {-# INLINE (>>=) #-}

    fail = Fail.fail
    {-# INLINE fail #-}

instance Fail.MonadFail Result where
    fail err = Error err
    {-# INLINE fail #-}

instance Applicative IResult where
    pure  = ISuccess
    {-# INLINE pure #-}
    (<*>) = ap
    {-# INLINE (<*>) #-}

instance Applicative Result where
    pure  = Success
    {-# INLINE pure #-}
    (<*>) = ap
    {-# INLINE (<*>) #-}

instance MonadPlus IResult where
    mzero = fail "mzero"
    {-# INLINE mzero #-}
    mplus a@(ISuccess _) _ = a
    mplus _ b             = b
    {-# INLINE mplus #-}

instance MonadPlus Result where
    mzero = fail "mzero"
    {-# INLINE mzero #-}
    mplus a@(Success _) _ = a
    mplus _ b             = b
    {-# INLINE mplus #-}

instance Alternative IResult where
    empty = mzero
    {-# INLINE empty #-}
    (<|>) = mplus
    {-# INLINE (<|>) #-}

instance Alternative Result where
    empty = mzero
    {-# INLINE empty #-}
    (<|>) = mplus
    {-# INLINE (<|>) #-}

instance Semigroup (IResult a) where
    (<>) = mplus
    {-# INLINE (<>) #-}

instance Monoid (IResult a) where
    mempty  = fail "mempty"
    {-# INLINE mempty #-}
    mappend = (<>)
    {-# INLINE mappend #-}

instance Semigroup (Result a) where
    (<>) = mplus
    {-# INLINE (<>) #-}

instance Monoid (Result a) where
    mempty  = fail "mempty"
    {-# INLINE mempty #-}
    mappend = (<>)
    {-# INLINE mappend #-}

instance Foldable IResult where
    foldMap _ (IError _ _) = mempty
    foldMap f (ISuccess y) = f y
    {-# INLINE foldMap #-}

    foldr _ z (IError _ _) = z
    foldr f z (ISuccess y) = f y z
    {-# INLINE foldr #-}

instance Foldable Result where
    foldMap _ (Error _)   = mempty
    foldMap f (Success y) = f y
    {-# INLINE foldMap #-}

    foldr _ z (Error _)   = z
    foldr f z (Success y) = f y z
    {-# INLINE foldr #-}

instance Traversable IResult where
    traverse _ (IError path err) = pure (IError path err)
    traverse f (ISuccess a)      = ISuccess <$> f a
    {-# INLINE traverse #-}

instance Traversable Result where
    traverse _ (Error err) = pure (Error err)
    traverse f (Success a) = Success <$> f a
    {-# INLINE traverse #-}

-- | Failure continuation.
type Failure f r   = JSONPath -> String -> f r
-- | Success continuation.
type Success a f r = a -> f r

-- | A JSON parser.
newtype Parser a = Parser {
      runParser :: forall f r.
                   JSONPath
                -> Failure f r
                -> Success a f r
                -> f r
    }

instance Monad Parser where
    m >>= g = Parser $ \path kf ks -> let ks' a = runParser (g a) path kf ks
                                       in runParser m path kf ks'
    {-# INLINE (>>=) #-}
    return = pure
    {-# INLINE return #-}
    fail = Fail.fail
    {-# INLINE fail #-}

instance Fail.MonadFail Parser where
    fail msg = Parser $ \path kf _ks -> kf (reverse path) msg
    {-# INLINE fail #-}

instance Functor Parser where
    fmap f m = Parser $ \path kf ks -> let ks' a = ks (f a)
                                        in runParser m path kf ks'
    {-# INLINE fmap #-}

instance Applicative Parser where
    pure a = Parser $ \_path _kf ks -> ks a
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
    mplus a b = Parser $ \path kf ks -> let kf' _ _ = runParser b path kf ks
                                         in runParser a path kf' ks
    {-# INLINE mplus #-}

instance Semigroup (Parser a) where
    (<>) = mplus
    {-# INLINE (<>) #-}

instance Monoid (Parser a) where
    mempty  = fail "mempty"
    {-# INLINE mempty #-}
    mappend = (<>)
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
           | Number !Scientific
           | Bool !Bool
           | Null
             deriving (Eq, Read, Show, Typeable, Data)

-- | A newtype wrapper for 'UTCTime' that uses the same non-standard
-- serialization format as Microsoft .NET, whose
-- <https://msdn.microsoft.com/en-us/library/system.datetime(v=vs.110).aspx System.DateTime>
-- type is by default serialized to JSON as in the following example:
--
-- > /Date(1302547608878)/
--
-- The number represents milliseconds since the Unix epoch.
newtype DotNetTime = DotNetTime {
      fromDotNetTime :: UTCTime
      -- ^ Acquire the underlying value.
    } deriving (Eq, Ord, Read, Show, Typeable, FormatTime)

instance NFData Value where
    rnf (Object o) = rnf o
    rnf (Array a)  = foldl' (\x y -> rnf y `seq` x) () a
    rnf (String s) = rnf s
    rnf (Number n) = rnf n
    rnf (Bool b)   = rnf b
    rnf Null       = ()

instance IsString Value where
    fromString = String . pack
    {-# INLINE fromString #-}

hashValue :: Int -> Value -> Int
#if MIN_VERSION_unordered_containers(0,2,6)
hashValue s (Object o)   = s `hashWithSalt` (0::Int) `hashWithSalt` o
#else
hashValue s (Object o)   = foldl' hashWithSalt
                              (s `hashWithSalt` (0::Int)) assocHashesSorted
  where
    assocHashesSorted = sort [hash k `hashWithSalt` v | (k, v) <- H.toList o]
#endif
hashValue s (Array a)    = foldl' hashWithSalt
                              (s `hashWithSalt` (1::Int)) a
hashValue s (String str) = s `hashWithSalt` (2::Int) `hashWithSalt` str
hashValue s (Number n)   = s `hashWithSalt` (3::Int) `hashWithSalt` n
hashValue s (Bool b)     = s `hashWithSalt` (4::Int) `hashWithSalt` b
hashValue s Null         = s `hashWithSalt` (5::Int)

instance Hashable Value where
    hashWithSalt = hashValue

-- @since 0.11.0.0
instance TH.Lift Value where
    lift Null = [| Null |]
    lift (Bool b) = [| Bool b |]
    lift (Number n) = [| Number (S.scientific c e) |]
      where
        c = S.coefficient n
        e = S.base10Exponent n
    lift (String t) = [| String (pack s) |]
      where s = unpack t
    lift (Array a) = [| Array (V.fromList a') |]
      where a' = V.toList a
    lift (Object o) = [| Object (H.fromList . map (first pack) $ o') |]
      where o' = map (first unpack) . H.toList $ o

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
parse m v = runParser (m v) [] (const Error) Success
{-# INLINE parse #-}

-- | Run a 'Parser'.
iparse :: (a -> Parser b) -> a -> IResult b
iparse m v = runParser (m v) [] IError ISuccess
{-# INLINE iparse #-}

-- | Run a 'Parser' with a 'Maybe' result type.
parseMaybe :: (a -> Parser b) -> a -> Maybe b
parseMaybe m v = runParser (m v) [] (\_ _ -> Nothing) Just
{-# INLINE parseMaybe #-}

-- | Run a 'Parser' with an 'Either' result type.  If the parse fails,
-- the 'Left' payload will contain an error message.
parseEither :: (a -> Parser b) -> a -> Either String b
parseEither m v = runParser (m v) [] onError Right
  where onError path msg = Left (formatError path msg)
{-# INLINE parseEither #-}

-- | Annotate an error message with a
-- <http://goessner.net/articles/JsonPath/ JSONPath> error location.
formatError :: JSONPath -> String -> String
formatError path msg = "Error in " ++ format "$" path ++ ": " ++ msg
  where
    format :: String -> JSONPath -> String
    format pfx []                = pfx
    format pfx (Index idx:parts) = format (pfx ++ "[" ++ show idx ++ "]") parts
    format pfx (Key key:parts)   = format (pfx ++ formatKey key) parts

    formatKey :: Text -> String
    formatKey key
       | isIdentifierKey strKey = "." ++ strKey
       | otherwise              = "['" ++ escapeKey strKey ++ "']"
      where strKey = unpack key

    isIdentifierKey :: String -> Bool
    isIdentifierKey []     = False
    isIdentifierKey (x:xs) = isAlpha x && all isAlphaNum xs

    escapeKey :: String -> String
    escapeKey = concatMap escapeChar

    escapeChar :: Char -> String
    escapeChar '\'' = "\\'"
    escapeChar '\\' = "\\\\"
    escapeChar c    = [c]

-- | A key\/value pair for an 'Object'.
type Pair = (Text, Value)

-- | Create a 'Value' from a list of name\/value 'Pair's.  If duplicate
-- keys arise, earlier keys and their associated values win.
object :: [Pair] -> Value
object = Object . H.fromList
{-# INLINE object #-}

-- | Add JSON Path context to a parser
--
-- When parsing a complex structure, it helps to annotate (sub)parsers
-- with context, so that if an error occurs, you can find its location.
--
-- > withObject "Person" $ \o ->
-- >   Person
-- >     <$> o .: "name" <?> Key "name"
-- >     <*> o .: "age"  <?> Key "age"
--
-- (Standard methods like '(.:)' already do this.)
--
-- With such annotations, if an error occurs, you will get a JSON Path
-- location of that error.
--
-- Since 0.10
(<?>) :: Parser a -> JSONPathElement -> Parser a
p <?> pathElem = Parser $ \path kf ks -> runParser p (pathElem:path) kf ks

-- | If the inner @Parser@ failed, modify the failure message using the
-- provided function. This allows you to create more descriptive error messages.
-- For example:
--
-- > parseJSON (Object o) = modifyFailure
-- >     ("Parsing of the Foo value failed: " ++)
-- >     (Foo <$> o .: "someField")
--
-- Since 0.6.2.0
modifyFailure :: (String -> String) -> Parser a -> Parser a
modifyFailure f (Parser p) = Parser $ \path kf ks -> p path (\p' m -> kf p' (f m)) ks

--------------------------------------------------------------------------------
-- Generic and TH encoding configuration
--------------------------------------------------------------------------------

-- | Options that specify how to encode\/decode your datatype to\/from JSON.
data Options = Options
    { fieldLabelModifier :: String -> String
      -- ^ Function applied to field labels.
      -- Handy for removing common record prefixes for example.
    , constructorTagModifier :: String -> String
      -- ^ Function applied to constructor tags which could be handy
      -- for lower-casing them for example.
    , allNullaryToStringTag :: Bool
      -- ^ If 'True' the constructors of a datatype, with /all/
      -- nullary constructors, will be encoded to just a string with
      -- the constructor tag. If 'False' the encoding will always
      -- follow the `sumEncoding`.
    , omitNothingFields :: Bool
      -- ^ If 'True' record fields with a 'Nothing' value will be
      -- omitted from the resulting object. If 'False' the resulting
      -- object will include those fields mapping to @null@.
    , sumEncoding :: SumEncoding
      -- ^ Specifies how to encode constructors of a sum datatype.
    , unwrapUnaryRecords :: Bool
      -- ^ Hide the field name when a record constructor has only one
      -- field, like a newtype.
    }

instance Show Options where
  show Options{..} = "Options {" ++
    "fieldLabelModifier =~ " ++
      show (fieldLabelModifier "exampleField") ++ ", " ++
    "constructorTagModifier =~ " ++
      show (constructorTagModifier "ExampleConstructor") ++ ", " ++
    "allNullaryToStringTag = " ++ show allNullaryToStringTag ++ ", " ++
    "omitNothingFields = " ++ show omitNothingFields ++ ", " ++
    "sumEncoding = " ++ show sumEncoding ++ ", " ++
    "unwrapUnaryRecords = " ++ show unwrapUnaryRecords ++
    "}"

-- | Specifies how to encode constructors of a sum datatype.
data SumEncoding =
    TaggedObject { tagFieldName      :: String
                 , contentsFieldName :: String
                 }
    -- ^ A constructor will be encoded to an object with a field
    -- 'tagFieldName' which specifies the constructor tag (modified by
    -- the 'constructorTagModifier'). If the constructor is a record
    -- the encoded record fields will be unpacked into this object. So
    -- make sure that your record doesn't have a field with the same
    -- label as the 'tagFieldName'. Otherwise the tag gets overwritten
    -- by the encoded value of that field! If the constructor is not a
    -- record the encoded constructor contents will be stored under
    -- the 'contentsFieldName' field.
  | UntaggedValue
    -- ^ Constructor names won't be encoded. Instead only the contents of the
    -- constructor will be encoded as if the type had single constructor. JSON
    -- encodings have to be disjoint for decoding to work properly.
    --
    -- When decoding, constructors are tried in the order of definition. If some
    -- encodings overlap, the first one defined will succeed.
    --
    -- /Note:/ Nullary constructors are encoded as the string (using
    -- 'constructorTagModifier'). Having a nullary constructor alongside a
    -- single field constructor that encodes to a string leads to ambiguity.
    --
    -- /Note:/ Only the last error is kept when decoding, so in the case of
    -- mailformed JSON, only an error for the last constructor will be reported.
  | ObjectWithSingleField
    -- ^ A constructor will be encoded to an object with a single
    -- field named after the constructor tag (modified by the
    -- 'constructorTagModifier') which maps to the encoded contents of
    -- the constructor.
  | TwoElemArray
    -- ^ A constructor will be encoded to a 2-element array where the
    -- first element is the tag of the constructor (modified by the
    -- 'constructorTagModifier') and the second element the encoded
    -- contents of the constructor.
    deriving (Eq, Show)

-- | Default encoding 'Options':
--
-- @
-- 'Options'
-- { 'fieldLabelModifier'      = id
-- , 'constructorTagModifier'  = id
-- , 'allNullaryToStringTag'   = True
-- , 'omitNothingFields'       = False
-- , 'sumEncoding'             = 'defaultTaggedObject'
-- , 'unwrapUnaryRecords'      = False
-- }
-- @
defaultOptions :: Options
defaultOptions = Options
                 { fieldLabelModifier      = id
                 , constructorTagModifier  = id
                 , allNullaryToStringTag   = True
                 , omitNothingFields       = False
                 , sumEncoding             = defaultTaggedObject
                 , unwrapUnaryRecords      = False
                 }

-- | Default 'TaggedObject' 'SumEncoding' options:
--
-- @
-- defaultTaggedObject = 'TaggedObject'
--                       { 'tagFieldName'      = \"tag\"
--                       , 'contentsFieldName' = \"contents\"
--                       }
-- @
defaultTaggedObject :: SumEncoding
defaultTaggedObject = TaggedObject
                      { tagFieldName      = "tag"
                      , contentsFieldName = "contents"
                      }

-- | Converts from CamelCase to another lower case, interspersing
--   the character between all capital letters and their previous
--   entries, except those capital letters that appear together,
--   like 'API'.
--
--   For use by Aeson template haskell calls.
--
--   > camelTo '_' 'CamelCaseAPI' == "camel_case_api"
camelTo :: Char -> String -> String
{-# DEPRECATED camelTo "Use camelTo2 for better results" #-}
camelTo c = lastWasCap True
  where
    lastWasCap :: Bool    -- ^ Previous was a capital letter
              -> String  -- ^ The remaining string
              -> String
    lastWasCap _    []           = []
    lastWasCap prev (x : xs)     = if isUpper x
                                      then if prev
                                             then toLower x : lastWasCap True xs
                                             else c : toLower x : lastWasCap True xs
                                      else x : lastWasCap False xs

-- | Better version of 'camelTo'. Example where it works better:
--
--   > camelTo '_' 'CamelAPICase' == "camel_apicase"
--   > camelTo2 '_' 'CamelAPICase' == "camel_api_case"
camelTo2 :: Char -> String -> String
camelTo2 c = map toLower . go2 . go1
    where go1 "" = ""
          go1 (x:u:l:xs) | isUpper u && isLower l = x : c : u : l : go1 xs
          go1 (x:xs) = x : go1 xs
          go2 "" = ""
          go2 (l:u:xs) | isLower l && isUpper u = l : c : u : go2 xs
          go2 (x:xs) = x : go2 xs
