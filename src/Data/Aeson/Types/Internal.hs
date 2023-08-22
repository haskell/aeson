{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

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
    , Key
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
    , iparseEither
    , parse
    , parseEither
    , parseMaybe
    , parseFail
    , modifyFailure
    , prependFailure
    , parserThrowError
    , parserCatchError
    , formatError
    , formatPath
    , formatRelativePath
    , (<?>)
    -- * Constructors and accessors
    , object

    -- * Generic and TH encoding configuration
    , Options(
          fieldLabelModifier
        , constructorTagModifier
        , allNullaryToStringTag
        , nullaryToObject
        , omitNothingFields
        , allowOmittedFields
        , sumEncoding
        , unwrapUnaryRecords
        , tagSingleConstructors
        , rejectUnknownFields
        )

    , SumEncoding(..)
    , JSONKeyOptions(keyModifier)
    , defaultOptions
    , defaultTaggedObject
    , defaultJSONKeyOptions

    -- * Used for changing CamelCase names into something else.
    , camelTo
    , camelTo2

    -- * Aeson Exception
    , AesonException (..)

    -- * Other types
    , DotNetTime(..)
    ) where

import Data.Aeson.Internal.Prelude

import Control.DeepSeq (NFData(..))
import Control.Exception (Exception (..))
import Control.Monad (MonadPlus(..), ap)
import Data.Char (isLower, isUpper, toLower, isAlpha, isAlphaNum)
import Data.Aeson.Key (Key)
import Data.Hashable (Hashable(..))
import Data.List (intercalate)
import Data.Text (pack, unpack)
import Data.Time.Format (FormatTime)
import Data.Aeson.KeyMap (KeyMap)
import qualified Control.Monad as Monad
import qualified Control.Monad.Fail as Fail
import qualified Data.Vector as V
import qualified Language.Haskell.TH.Syntax as TH
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Scientific as Sci
import qualified Data.Text as T
import qualified Test.QuickCheck as QC
import Witherable (ordNub)

-- | Elements of a JSON path used to describe the location of an
-- error.
data JSONPathElement = Key Key
                       -- ^ JSON path element of a key into an object,
                       -- \"object.key\".
                     | Index {-# UNPACK #-} !Int
                       -- ^ JSON path element of an index into an
                       -- array, \"array[index]\".
                       deriving (Eq, Show, Typeable, Ord)
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

instance Monad.Monad IResult where
    return = pure
    {-# INLINE return #-}

    ISuccess a      >>= k = k a
    IError path err >>= _ = IError path err
    {-# INLINE (>>=) #-}

#if !(MIN_VERSION_base(4,13,0))
    fail = Fail.fail
    {-# INLINE fail #-}
#endif

instance Fail.MonadFail IResult where
    fail err = IError [] err
    {-# INLINE fail #-}

instance Monad.Monad Result where
    return = pure
    {-# INLINE return #-}

    Success a >>= k = k a
    Error err >>= _ = Error err
    {-# INLINE (>>=) #-}

#if !(MIN_VERSION_base(4,13,0))
    fail = Fail.fail
    {-# INLINE fail #-}
#endif

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

-- | A JSON parser.  N.B. This might not fit your usual understanding of
--  "parser".  Instead you might like to think of 'Parser' as a "parse result",
-- i.e. a parser to which the input has already been applied.
newtype Parser a = Parser {
      runParser :: forall f r.
                   JSONPath
                -> Failure f r
                -> Success a f r
                -> f r
    }

instance Monad.Monad Parser where
    m >>= g = Parser $ \path kf ks -> let ks' a = runParser (g a) path kf ks
                                       in runParser m path kf ks'
    {-# INLINE (>>=) #-}
    return = pure
    {-# INLINE return #-}

#if !(MIN_VERSION_base(4,13,0))
    fail = Fail.fail
    {-# INLINE fail #-}
#endif

-- |
--
-- @since 2.1.0.0
instance MonadFix Parser where
    mfix f = Parser $ \path kf ks -> let x = runParser (f (fromISuccess x)) path IError ISuccess in
        case x of
            IError p e -> kf p e
            ISuccess y -> ks y
      where
        fromISuccess :: IResult a -> a
        fromISuccess (ISuccess x)      = x
        fromISuccess (IError path msg) = error $ "mfix @Aeson.Parser: " ++ formatPath path ++ ": " ++ msg

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

-- | Raise a parsing failure with some custom message.
parseFail :: String -> Parser a
parseFail = fail

apP :: Parser (a -> b) -> Parser a -> Parser b
apP d e = do
  b <- d
  b <$> e
{-# INLINE apP #-}

-- | A JSON \"object\" (key\/value map).
type Object = KeyMap Value

-- | A JSON \"array\" (sequence).
type Array = Vector Value

-- | A JSON value represented as a Haskell value.
data Value = Object !Object
           | Array !Array
           | String !Text
           | Number !Scientific
           | Bool !Bool
           | Null
             deriving (Eq, Read, Typeable, Data, Generic)

-- | Since version 1.5.6.0 version object values are printed in lexicographic key order
--
-- >>> toJSON $ H.fromList [("a", True), ("z", False)]
-- Object (fromList [("a",Bool True),("z",Bool False)])
--
-- >>> toJSON $ H.fromList [("z", False), ("a", True)]
-- Object (fromList [("a",Bool True),("z",Bool False)])
--
instance Show Value where
    showsPrec _ Null = showString "Null"
    showsPrec d (Bool b) = showParen (d > 10)
        $ showString "Bool " . showsPrec 11 b
    showsPrec d (Number s) = showParen (d > 10)
        $ showString "Number " . showsPrec 11 s
    showsPrec d (String s) = showParen (d > 10)
        $ showString "String " . showsPrec 11 s
    showsPrec d (Array xs) = showParen (d > 10)
        $ showString "Array " . showsPrec 11 xs
    showsPrec d (Object xs) = showParen (d > 10)
        $ showString "Object (fromList "
        . showsPrec 11 (KM.toAscList xs)
        . showChar ')'

-- | @since 2.0.3.0
instance QC.Arbitrary Value where
    arbitrary = QC.sized arbValue

    shrink = ordNub . go where
        go Null       = []
        go (Bool b)   = Null : map Bool (QC.shrink b)
        go (String x) = Null : map (String . T.pack) (QC.shrink (T.unpack x))
        go (Number x) = Null : map Number (shrScientific x)
        go (Array x)  = Null : V.toList x ++ map (Array . V.fromList) (QC.liftShrink go (V.toList x))
        go (Object x) = Null : KM.elems x ++ map (Object . KM.fromList) (QC.liftShrink (QC.liftShrink go) (KM.toList x))

-- | @since 2.0.3.0
instance QC.CoArbitrary Value where
    coarbitrary Null       = QC.variant (0 :: Int)
    coarbitrary (Bool b)   = QC.variant (1 :: Int) . QC.coarbitrary b
    coarbitrary (String x) = QC.variant (2 :: Int) . QC.coarbitrary (T.unpack x)
    coarbitrary (Number x) = QC.variant (3 :: Int) . QC.coarbitrary (Sci.coefficient x) . QC.coarbitrary (Sci.base10Exponent x)
    coarbitrary (Array x)  = QC.variant (4 :: Int) . QC.coarbitrary (V.toList x)
    coarbitrary (Object x) = QC.variant (5 :: Int) . QC.coarbitrary (KM.toList x)

-- | @since 2.0.3.0
instance QC.Function Value where
    function = QC.functionMap fwd bwd where
        fwd :: Value -> RepValue
        fwd Null       = Left Nothing
        fwd (Bool b)   = Left (Just b)
        fwd (String x) = Right (Left (Left (T.unpack x)))
        fwd (Number x) = Right (Left (Right (Sci.coefficient x, Sci.base10Exponent x)))
        fwd (Array x)  = Right (Right (Left (V.toList x)))
        fwd (Object x) = Right (Right (Right (KM.toList x)))

        bwd :: RepValue -> Value
        bwd (Left Nothing)                = Null
        bwd (Left (Just b))               = Bool b
        bwd (Right (Left (Left x)))       = String (T.pack x)
        bwd (Right (Left (Right (x, y)))) = Number (Sci.scientific x y)
        bwd (Right (Right (Left x)))      = Array (V.fromList x)
        bwd (Right (Right (Right x)))     = Object (KM.fromList x)

-- Used to implement QC.Function Value instance
type RepValue
    = Either (Maybe Bool) (Either (Either String (Integer, Int)) (Either [Value] [(Key, Value)]))

arbValue :: Int -> QC.Gen Value
arbValue n
    | n <= 1 = QC.oneof
        [ pure Null
        , Bool <$> QC.arbitrary
        , String <$> arbText
        , Number <$> arbScientific
        , pure emptyObject
        , pure emptyArray
        ]

    | otherwise = QC.oneof
        [ Object <$> arbObject n
        , Array <$> arbArray  n
        ]

arbText :: QC.Gen Text
arbText = T.pack <$> QC.arbitrary

arbScientific :: QC.Gen Scientific
arbScientific = Sci.scientific <$> QC.arbitrary <*> QC.arbitrary

shrScientific :: Scientific -> [Scientific]
shrScientific s = map (uncurry Sci.scientific) $
    QC.shrink (Sci.coefficient s, Sci.base10Exponent s) 

arbObject :: Int -> QC.Gen Object
arbObject n = do
    p <- arbPartition (n - 1)
    KM.fromList <$> traverse (\m -> (,) <$> QC.arbitrary <*> arbValue m) p

arbArray :: Int -> QC.Gen Array
arbArray n = do
    p <- arbPartition (n - 1)
    V.fromList <$> traverse arbValue p

arbPartition :: Int -> QC.Gen [Int]
arbPartition k = case compare k 1 of
    LT -> pure []
    EQ -> pure [1]
    GT -> do
        first <- QC.chooseInt (1, k)
        rest <- arbPartition $ k - first
        QC.shuffle (first : rest)

-- |
--
-- The ordering is total, consistent with 'Eq' instance.
-- However, nothing else about the ordering is specified,
-- and it may change from environment to environment and version to version
-- of either this package or its dependencies ('hashable' and 'unordered-containers').
--
-- @since 1.5.2.0
deriving instance Ord Value
-- standalone deriving to attach since annotation.

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
hashValue s (Object o)   = s `hashWithSalt` (0::Int) `hashWithSalt` o
hashValue s (Array a)    = foldl' hashWithSalt
                              (s `hashWithSalt` (1::Int)) a
hashValue s (String str) = s `hashWithSalt` (2::Int) `hashWithSalt` str
hashValue s (Number n)   = s `hashWithSalt` (3::Int) `hashWithSalt` n
hashValue s (Bool b)     = s `hashWithSalt` (4::Int) `hashWithSalt` b
hashValue s Null         = s `hashWithSalt` (5::Int)

instance Hashable Value where
    hashWithSalt = hashValue

-- | @since 0.11.0.0
instance TH.Lift Value where
    lift Null       = [| Null |]
    lift (Bool b)   = [| Bool b |]
    lift (Number n) = [| Number n |]
    lift (String t) = [| String (pack s) |]
      where s = unpack t
    lift (Array a)  = [| Array (V.fromList a') |]
      where a' = V.toList a
    lift (Object o) = [| Object o |]

#if MIN_VERSION_template_haskell(2,17,0)
    liftTyped = TH.unsafeCodeCoerce . TH.lift
#elif MIN_VERSION_template_haskell(2,16,0)
    liftTyped = TH.unsafeTExpCoerce . TH.lift
#endif

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
emptyObject = Object KM.empty

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

-- | Run a 'Parser' with an 'Either' result type.
-- If the parse fails, the 'Left' payload will contain an error message and a json path to failed element.
--
-- @since 2.1.0.0
iparseEither :: (a -> Parser b) -> a -> Either (JSONPath, String) b
iparseEither m v = runParser (m v) [] (\path msg -> Left (path, msg)) Right
{-# INLINE iparseEither #-}

-- | Annotate an error message with a
-- <http://goessner.net/articles/JsonPath/ JSONPath> error location.
formatError :: JSONPath -> String -> String
formatError path msg = "Error in " ++ formatPath path ++ ": " ++ msg

-- | Format a <http://goessner.net/articles/JsonPath/ JSONPath> as a 'String',
-- representing the root object as @$@.
formatPath :: JSONPath -> String
formatPath path = "$" ++ formatRelativePath path

-- | Format a <http://goessner.net/articles/JsonPath/ JSONPath> as a 'String'
-- which represents the path relative to some root object.
formatRelativePath :: JSONPath -> String
formatRelativePath path = format "" path
  where
    format :: String -> JSONPath -> String
    format pfx []                = pfx
    format pfx (Index idx:parts) = format (pfx ++ "[" ++ show idx ++ "]") parts
    format pfx (Key key:parts)   = format (pfx ++ formatKey key) parts

    formatKey :: Key -> String
    formatKey key
       | isIdentifierKey strKey = "." ++ strKey
       | otherwise              = "['" ++ escapeKey strKey ++ "']"
      where strKey = Key.toString key

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
type Pair = (Key, Value)

-- | Create a 'Value' from a list of name\/value 'Pair's.  If duplicate
-- keys arise, later keys and their associated values win.
object :: [Pair] -> Value
object = Object . KM.fromList
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
modifyFailure f (Parser p) = Parser $ \path kf ks ->
    p path (\p' m -> kf p' (f m)) ks

-- | If the inner 'Parser' failed, prepend the given string to the failure
-- message.
--
-- @
-- 'prependFailure' s = 'modifyFailure' (s '++')
-- @
prependFailure :: String -> Parser a -> Parser a
prependFailure = modifyFailure . (++)

-- | Throw a parser error with an additional path.
--
-- @since 1.2.1.0
parserThrowError :: JSONPath -> String -> Parser a
parserThrowError path' msg = Parser $ \path kf _ks ->
    kf (reverse path ++ path') msg

-- | A handler function to handle previous errors and return to normal execution.
--
-- @since 1.2.1.0
parserCatchError :: Parser a -> (JSONPath -> String -> Parser a) -> Parser a
parserCatchError (Parser p) handler = Parser $ \path kf ks ->
    p path (\e msg -> runParser (handler e msg) path kf ks) ks

--------------------------------------------------------------------------------
-- Generic and TH encoding configuration
--------------------------------------------------------------------------------

-- | Options that specify how to encode\/decode your datatype to\/from JSON.
--
-- Options can be set using record syntax on 'defaultOptions' with the fields
-- below.
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
    , nullaryToObject :: Bool
      -- ^ If 'True', the nullary constructors will be encoded
      -- as empty objects (the default is to encode them as empty arrays).
    , omitNothingFields :: Bool
      -- ^ If 'True', record fields with a 'Nothing' value will be
      -- omitted from the resulting object. If 'False', the resulting
      -- object will include those fields mapping to @null@.
      --
      -- In @aeson-2.2@ this flag is generalised to omit all values with @'Data.Aeson.Types.omitField' x = True@.
      -- If 'False', the resulting object will include those fields encoded as specified. 
      --
      -- Note that this /does not/ affect parsing: 'Maybe' fields are
      -- optional regardless of the value of 'omitNothingFields'.
      -- 'allowOmittedFieds' controls parsing behavior.
    , allowOmittedFields :: Bool
      -- ^ If 'True', missing fields of a record will be filled
      -- with 'omittedField' values (if they are 'Just').
      -- If 'False', all fields will required to present in the record object.
    , sumEncoding :: SumEncoding
      -- ^ Specifies how to encode constructors of a sum datatype.
    , unwrapUnaryRecords :: Bool
      -- ^ Hide the field name when a record constructor has only one
      -- field, like a newtype.
    , tagSingleConstructors :: Bool
      -- ^ Encode types with a single constructor as sums,
      -- so that `allNullaryToStringTag` and `sumEncoding` apply.
    , rejectUnknownFields :: Bool
      -- ^ Applies only to 'Data.Aeson.FromJSON' instances. If a field appears in
      -- the parsed object map, but does not appear in the target object, parsing
      -- will fail, with an error message indicating which fields were unknown.
    }

instance Show Options where
  show (Options f c a n o q s u t r) =
       "Options {"
    ++ intercalate ", "
      [ "fieldLabelModifier =~ " ++ show (f "exampleField")
      , "constructorTagModifier =~ " ++ show (c "ExampleConstructor")
      , "allNullaryToStringTag = " ++ show a
      , "nullaryToObject = " ++ show n
      , "omitNothingFields = " ++ show o
      , "allowOmittedFields = " ++ show q
      , "sumEncoding = " ++ show s
      , "unwrapUnaryRecords = " ++ show u
      , "tagSingleConstructors = " ++ show t
      , "rejectUnknownFields = " ++ show r
      ]
    ++ "}"

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
    -- constructor will be encoded as if the type had a single constructor. JSON
    -- encodings have to be disjoint for decoding to work properly.
    --
    -- When decoding, constructors are tried in the order of definition. If some
    -- encodings overlap, the first one defined will succeed.
    --
    -- /Note:/ Nullary constructors are encoded as strings (using
    -- 'constructorTagModifier'). Having a nullary constructor alongside a
    -- single field constructor that encodes to a string leads to ambiguity.
    --
    -- /Note:/ Only the last error is kept when decoding, so in the case of
    -- malformed JSON, only an error for the last constructor will be reported.
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

-- | Options for encoding keys with 'Data.Aeson.Types.genericFromJSONKey' and
-- 'Data.Aeson.Types.genericToJSONKey'.
data JSONKeyOptions = JSONKeyOptions
    { keyModifier :: String -> String
      -- ^ Function applied to keys. Its result is what goes into the encoded
      -- 'Value'.
      --
      -- === __Example__
      --
      -- The following instances encode the constructor @Bar@ to lower-case keys
      -- @\"bar\"@.
      --
      -- @
      -- data Foo = Bar
      --   deriving 'Generic'
      --
      -- opts :: 'JSONKeyOptions'
      -- opts = 'defaultJSONKeyOptions' { 'keyModifier' = 'toLower' }
      --
      -- instance 'ToJSONKey' Foo where
      --   'toJSONKey' = 'genericToJSONKey' opts
      --
      -- instance 'FromJSONKey' Foo where
      --   'fromJSONKey' = 'genericFromJSONKey' opts
      -- @
    }

-- | Default encoding 'Options':
--
-- @
-- 'Options'
-- { 'fieldLabelModifier'      = id
-- , 'constructorTagModifier'  = id
-- , 'allNullaryToStringTag'   = True
-- , 'omitNothingFields'       = False
-- , 'allowOmittedFields'      = True
-- , 'sumEncoding'             = 'defaultTaggedObject'
-- , 'unwrapUnaryRecords'      = False
-- , 'tagSingleConstructors'   = False
-- , 'rejectUnknownFields'     = False
-- }
-- @
defaultOptions :: Options
defaultOptions = Options
                 { fieldLabelModifier      = id
                 , constructorTagModifier  = id
                 , allNullaryToStringTag   = True
                 , nullaryToObject         = False
                 , omitNothingFields       = False
                 , allowOmittedFields      = True
                 , sumEncoding             = defaultTaggedObject
                 , unwrapUnaryRecords      = False
                 , tagSingleConstructors   = False
                 , rejectUnknownFields     = False
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

-- | Default 'JSONKeyOptions':
--
-- @
-- defaultJSONKeyOptions = 'JSONKeyOptions'
--                         { 'keyModifier' = 'id'
--                         }
-- @
defaultJSONKeyOptions :: JSONKeyOptions
defaultJSONKeyOptions = JSONKeyOptions id

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
--   > camelTo '_' "CamelAPICase" == "camel_apicase"
--   > camelTo2 '_' "CamelAPICase" == "camel_api_case"
camelTo2 :: Char -> String -> String
camelTo2 c = map toLower . go2 . go1
    where go1 "" = ""
          go1 (x:u:l:xs) | isUpper u && isLower l = x : c : u : l : go1 xs
          go1 (x:xs) = x : go1 xs
          go2 "" = ""
          go2 (l:u:xs) | isLower l && isUpper u = l : c : u : go2 xs
          go2 (x:xs) = x : go2 xs

-------------------------------------------------------------------------------
-- AesonException
-------------------------------------------------------------------------------

-- | Exception thrown by 'throwDecode' and variants.
--
-- @since 2.1.2.0
newtype AesonException = AesonException String
  deriving (Show)

instance Exception AesonException where
    displayException (AesonException str) = "aeson: " ++ str
