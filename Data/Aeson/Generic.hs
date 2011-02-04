module Data.Aeson.Generic
    (
      fromJSON
    , toJSON
    ) where

import Control.Applicative (Alternative)
import Data.Aeson.Functions
import Data.Aeson.Types hiding (FromJSON(..), ToJSON(..))
import Data.Generics
import Data.Int
import Data.IntSet (IntSet)
import Data.Text (Text, pack)
import Data.Time.Clock (UTCTime)
import Data.Word
import qualified Data.Aeson.Types as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text.Lazy as L
import qualified Data.Vector as V

type T a = a -> Value

toJSON :: (Data a) => a -> Value
toJSON = toJSON_generic
         `ext1Q` list
         `ext1Q` vector
         `ext1Q` set
         `ext1Q` mapText
         `ext1Q` mapLazyText
         `ext1Q` mapString
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
         `extQ` (T.toJSON :: T Float)
         `extQ` (T.toJSON :: T Rational)
         `extQ` (T.toJSON :: T Char)
         `extQ` (T.toJSON :: T Text)
         `extQ` (T.toJSON :: T L.Text)
         `extQ` (T.toJSON :: T String)
         `extQ` (T.toJSON :: T B.ByteString)
         `extQ` (T.toJSON :: T L.ByteString)
         `extQ` (T.toJSON :: T T.Value)
         `extQ` (T.toJSON :: T UTCTime)
         `extQ` (T.toJSON :: T IntSet)
         `extQ` (T.toJSON :: T Bool)
         `extQ` (T.toJSON :: T ())
         --`extQ` (T.toJSON :: T Ordering)
  where
    list xs = Array . V.fromList . map toJSON $ xs
    vector v = Array . V.map toJSON $ v
    set s = Array . V.fromList . map toJSON . Set.toList $ s
    mapText m = Object . Map.map toJSON $ m
    mapLazyText m = Object . transformMap L.toStrict toJSON $ m
    mapString m = Object . transformMap pack toJSON $ m

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
              err dt r = error $ "Data.Aeson.Generic.toJSON: not AlgRep " ++
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

fromJSON :: (Alternative f, Data a) => Value -> f a
fromJSON = undefined
