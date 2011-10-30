{-# LANGUAGE PatternGuards, RankNTypes, ScopedTypeVariables  #-}

-- |
-- Module:      Data.Aeson.Generic
-- Copyright:   (c) 2011 MailRank, Inc.
--              (c) 2008, 2009 Lennart Augustsson
-- License:     BSD3
-- Maintainer:  Bryan O'Sullivan <bos@mailrank.com>
-- Stability:   experimental
-- Portability: portable
--
-- JSON handling using 'Data.Generics'.
--
-- This is based on the 'Text.JSON.Generic' package originally written
-- by Lennart Augustsson.

module Data.Aeson.Generic
    (
      fromJSON
    , toJSON
    ) where

import Control.Applicative ((<$>))
import Control.Arrow (first)
import Control.Monad.State.Strict
import Data.Aeson.Functions
import Data.Aeson.Types hiding (FromJSON(..), ToJSON(..), fromJSON)
import Data.Attoparsec.Number (Number)
import Data.Generics
import Data.Hashable (Hashable)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.IntSet (IntSet)
import Data.Maybe (fromJust)
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock (UTCTime)
import Data.Word (Word, Word8, Word16, Word32, Word64)
import qualified Data.Aeson.Types as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.HashMap.Strict as H
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as DT
import qualified Data.Text.Lazy as LT
import qualified Data.Traversable as T
import qualified Data.Vector as V

type T a = a -> Value

toJSON :: (Data a) => a -> Value
toJSON = toJSON_generic
         `ext1Q` list
         `ext1Q` vector
         `ext1Q` set
         `ext2Q'` mapAny
         `ext2Q'` hashMapAny
         -- Use the standard encoding for all base types.
         `extQ` (T.toJSON :: T Integer)
         `extQ` (T.toJSON :: T Int)
         `extQ` (T.toJSON :: T Int8)
         `extQ` (T.toJSON :: T Int16)
         `extQ` (T.toJSON :: T Int32)
         `extQ` (T.toJSON :: T Int64)
         `extQ` (T.toJSON :: T Word)
         `extQ` (T.toJSON :: T Word8)
         `extQ` (T.toJSON :: T Word16)
         `extQ` (T.toJSON :: T Word32)
         `extQ` (T.toJSON :: T Word64)
         `extQ` (T.toJSON :: T Double)
         `extQ` (T.toJSON :: T Number)
         `extQ` (T.toJSON :: T Float)
         `extQ` (T.toJSON :: T Rational)
         `extQ` (T.toJSON :: T Char)
         `extQ` (T.toJSON :: T Text)
         `extQ` (T.toJSON :: T LT.Text)
         `extQ` (T.toJSON :: T String)
         `extQ` (T.toJSON :: T B.ByteString)
         `extQ` (T.toJSON :: T L.ByteString)
         `extQ` (T.toJSON :: T T.Value)
         `extQ` (T.toJSON :: T DotNetTime)
         `extQ` (T.toJSON :: T UTCTime)
         `extQ` (T.toJSON :: T IntSet)
         `extQ` (T.toJSON :: T Bool)
         `extQ` (T.toJSON :: T ())
         --`extQ` (T.toJSON :: T Ordering)
  where
    list xs = Array . V.fromList . map toJSON $ xs
    vector v = Array . V.map toJSON $ v
    set s = Array . V.fromList . map toJSON . Set.toList $ s

    mapAny m
      | tyrep == typeOf DT.empty = remap id
      | tyrep == typeOf LT.empty = remap LT.toStrict
      | tyrep == typeOf ""       = remap pack
      | tyrep == typeOf B.empty  = remap decode
      | tyrep == typeOf L.empty  = remap strict
      | otherwise = modError "toJSON" $
                             "cannot convert map keyed by type " ++ show tyrep
      where tyrep = typeOf . head . Map.keys $ m
            remap f = Object . transformMap (f . fromJust . cast) toJSON $ m

    hashMapAny m
      | tyrep == typeOf DT.empty = remap id
      | tyrep == typeOf LT.empty = remap LT.toStrict
      | tyrep == typeOf ""       = remap pack
      | tyrep == typeOf B.empty  = remap decode
      | tyrep == typeOf L.empty  = remap strict
      | otherwise = modError "toJSON" $
                             "cannot convert map keyed by type " ++ show tyrep
      where tyrep = typeOf . head . H.keys $ m
            remap f = Object . hashMap (f . fromJust . cast) toJSON $ m


toJSON_generic :: (Data a) => a -> Value
toJSON_generic = generic
  where
        -- Generic encoding of an algebraic data type.
        generic a =
            case dataTypeRep (dataTypeOf a) of
                -- No constructor, so it must be an error value.  Code
                -- it anyway as Null.
                AlgRep []  -> Null
                -- Elide a single constructor and just code the arguments.
                AlgRep [c] -> encodeArgs c (gmapQ toJSON a)
                -- For multiple constructors, make an object with a
                -- field name that is the constructor (except lower
                -- case) and the data is the arguments encoded.
                AlgRep _   -> encodeConstr (toConstr a) (gmapQ toJSON a)
                rep        -> err (dataTypeOf a) rep
           where
              err dt r = modError "toJSON" $ "not AlgRep " ++
                                  show r ++ "(" ++ show dt ++ ")"
        -- Encode nullary constructor as a string.
        -- Encode non-nullary constructors as an object with the constructor
        -- name as the single field and the arguments as the value.
        -- Use an array if the are no field names, but elide singleton arrays,
        -- and use an object if there are field names.
        encodeConstr c [] = String . constrString $ c
        encodeConstr c as = object [(constrString c, encodeArgs c as)]

        constrString = pack . showConstr

        encodeArgs c = encodeArgs' (constrFields c)
        encodeArgs' [] [j] = j
        encodeArgs' [] js  = Array . V.fromList $ js
        encodeArgs' ns js  = object $ zip (map mungeField ns) js

        -- Skip leading '_' in field name so we can use keywords
        -- etc. as field names.
        mungeField ('_':cs) = pack cs
        mungeField cs       = pack cs

fromJSON :: (Data a) => Value -> Result a
fromJSON = parse parseJSON

type F a = Parser a

parseJSON :: (Data a) => Value -> Parser a
parseJSON j = parseJSON_generic j
             `ext1R` list
             `ext1R` vector
             `ext2R'` mapAny
             `ext2R'` hashMapAny
             -- Use the standard encoding for all base types.
             `extR` (value :: F Integer)
             `extR` (value :: F Int)
             `extR` (value :: F Int8)
             `extR` (value :: F Int16)
             `extR` (value :: F Int32)
             `extR` (value :: F Int64)
             `extR` (value :: F Word)
             `extR` (value :: F Word8)
             `extR` (value :: F Word16)
             `extR` (value :: F Word32)
             `extR` (value :: F Word64)
             `extR` (value :: F Double)
             `extR` (value :: F Number)
             `extR` (value :: F Float)
             `extR` (value :: F Rational)
             `extR` (value :: F Char)
             `extR` (value :: F Text)
             `extR` (value :: F LT.Text)
             `extR` (value :: F String)
             `extR` (value :: F B.ByteString)
             `extR` (value :: F L.ByteString)
             `extR` (value :: F T.Value)
             `extR` (value :: F DotNetTime)
             `extR` (value :: F UTCTime)
             `extR` (value :: F IntSet)
             `extR` (value :: F Bool)
             `extR` (value :: F ())
  where
    value :: (T.FromJSON a) => Parser a
    value = T.parseJSON j
    list :: (Data a) => Parser [a]
    list = V.toList <$> parseJSON j
    vector :: (Data a) => Parser (V.Vector a)
    vector = case j of
               Array js -> V.mapM parseJSON js
               _        -> myFail
    mapAny :: forall e f. (Data e, Data f) => Parser (Map.Map f e)
    mapAny
        | tyrep `elem` stringyTypes = res
        | otherwise = myFail
      where res = case j of
                Object js -> Map.mapKeysMonotonic trans <$> T.mapM parseJSON js
                _         -> myFail
            trans
               | tyrep == typeOf DT.empty = remap id
               | tyrep == typeOf LT.empty = remap LT.fromStrict
               | tyrep == typeOf ""       = remap DT.unpack
               | tyrep == typeOf B.empty  = remap encodeUtf8
               | tyrep == typeOf L.empty  = remap lazy
               | otherwise = modError "parseJSON"
                                      "mapAny -- should never happen"
            tyrep = typeOf (undefined :: f)
            remap f = fromJust . cast . f
    hashMapAny :: forall e f. (Data e, Data f) => Parser (H.HashMap f e)
    hashMapAny
        | tyrep == typeOf ""       = process DT.unpack
        | tyrep == typeOf LT.empty = process LT.fromStrict
        | tyrep == typeOf DT.empty = process id
        | otherwise = myFail
      where
        process f = maybe myFail return . cast =<< parseWith f
        parseWith :: (Eq c, Hashable c) => (Text -> c) -> Parser (H.HashMap c e)
        parseWith f = case j of
                        Object js -> H.fromList . map (first f) . Map.toList <$>
                                     T.mapM parseJSON js
                        _          -> myFail
        tyrep = typeOf (undefined :: f)
    myFail = modFail "parseJSON" $ "bad data: " ++ show j
    stringyTypes = [typeOf LT.empty, typeOf DT.empty, typeOf B.empty,
                    typeOf L.empty, typeOf ""]

parseJSON_generic :: (Data a) => Value -> Parser a
parseJSON_generic j = generic
  where
        typ = dataTypeOf $ resType generic
        generic = case dataTypeRep typ of
                    AlgRep []  -> case j of
                                    Null -> return (modError "parseJSON" "empty type")
                                    _ -> modFail "parseJSON" "no-constr bad data"
                    AlgRep [_] -> decodeArgs (indexConstr typ 1) j
                    AlgRep _   -> do (c, j') <- getConstr typ j; decodeArgs c j'
                    rep        -> modFail "parseJSON" $
                                  show rep ++ "(" ++ show typ ++ ")"
        getConstr t (Object o) | [(s, j')] <- fromJSObject o = do
                                                c <- readConstr' t s
                                                return (c, j')
        getConstr t (String js) = do c <- readConstr' t (unpack js)
                                     return (c, Null) -- handle nullary ctor
        getConstr _ _ = modFail "parseJSON" "bad constructor encoding"
        readConstr' t s =
          maybe (modFail "parseJSON" $ "unknown constructor: " ++ s ++ " " ++
                         show t)
                return $ readConstr t s

        decodeArgs c0 = go (numConstrArgs (resType generic) c0) c0
                           (constrFields c0)
         where
          go 0 c  _       Null       = construct c []   -- nullary constructor
          go 1 c []       jd         = construct c [jd] -- unary constructor
          go n c []       (Array js)
              | n > 1 = construct c (V.toList js)   -- no field names
          -- FIXME? We could allow reading an array into a constructor
          -- with field names.
          go _ c fs@(_:_) (Object o) = selectFields o fs >>=
                                       construct c -- field names
          go _ c _        jd         = modFail "parseJSON" $
                                       "bad decodeArgs data " ++ show (c, jd)

        fromJSObject = map (first unpack) . Map.toList

        -- Build the value by stepping through the list of subparts.
        construct c = evalStateT $ fromConstrM f c
          where f :: (Data a) => StateT [Value] Parser a
                f = do js <- get
                       case js of
                         [] -> lift $ modFail "construct" "empty list"
                         (j':js') -> do put js'; lift $ parseJSON j'

        -- Select the named fields from a JSON object.
        selectFields fjs = mapM sel
          where sel f = maybe (modFail "parseJSON" $ "field does not exist " ++
                               f) return $ Map.lookup (pack f) fjs

        -- Count how many arguments a constructor has.  The value x is
        -- used to determine what type the constructor returns.
        numConstrArgs :: (Data a) => a -> Constr -> Int
        numConstrArgs x c = execState (fromConstrM f c `asTypeOf` return x) 0
          where f = do modify (+1); return undefined

        resType :: MonadPlus m => m a -> a
        resType _ = modError "parseJSON" "resType"

modFail :: (Monad m) => String -> String -> m a
modFail func err = fail $ "Data.Aeson.Generic." ++ func ++ ": " ++ err

modError :: String -> String -> a
modError func err = error $ "Data.Aeson.Generic." ++ func ++ ": " ++ err


-- Type extension for binary type constructors.

-- | Flexible type extension
ext2' :: (Data a, Typeable2 t)
     => c a
     -> (forall d1 d2. (Data d1, Data d2) => c (t d1 d2))
     -> c a
ext2' def ext = maybe def id (dataCast2 ext)

-- | Type extension of queries for type constructors
ext2Q' :: (Data d, Typeable2 t)
      => (d -> q)
      -> (forall d1 d2. (Data d1, Data d2) => t d1 d2 -> q)
      -> d -> q
ext2Q' def ext = unQ ((Q def) `ext2'` (Q ext))

-- | Type extension of readers for type constructors
ext2R' :: (Monad m, Data d, Typeable2 t)
      => m d
      -> (forall d1 d2. (Data d1, Data d2) => m (t d1 d2))
      -> m d
ext2R' def ext = unR ((R def) `ext2'` (R ext))

-- | The type constructor for queries
newtype Q q x = Q { unQ :: x -> q }

-- | The type constructor for readers
newtype R m x = R { unR :: m x }
