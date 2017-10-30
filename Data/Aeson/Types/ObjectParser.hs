{-# LANGUAGE GADTs, PatternGuards #-}
module Data.Aeson.Types.ObjectParser (
    ObjectParser,
    withObjectParser,
    withObjectParser',
    runObjectParser,
    runObjectParser',
    liftObjectParser,
    explicitObjectField,
    objectField,
    ) where

import Prelude ()
import Prelude.Compat

import Data.Text (Text)
import qualified Data.HashSet as HS

import Data.Aeson.Types.Internal
import Data.Aeson.Types.FromJSON

-- | Applicative Object parser
data ObjectParser a = OP
    { runObjectParser :: !(Object -> Parser a)
    , _opKeys         :: !(HS.HashSet Text)
    -- TODO: maybe fields
    }

instance Functor ObjectParser where
    fmap f (OP x ks) = OP (fmap f . x) ks
    {-# INLINE fmap #-}

instance Applicative ObjectParser where
    pure x = OP (\_ -> pure x) mempty
    OP f ks <*> OP x ks' = OP (\obj -> f obj <*> x obj) (HS.union ks ks')
    {-# INLINE (<*>) #-}

withObjectParser :: String -> ObjectParser a -> Value -> Parser a
withObjectParser name p = withObject name (runObjectParser p)

withObjectParser'
    :: String
    -> ObjectParser a
    -> HS.HashSet Text  -- ^ required
    -> HS.HashSet Text  -- ^ optional
    -> Value -> Parser a
withObjectParser' name p req opt = withObject name (runObjectParser' p req opt)

liftObjectParser :: Text -> (Value -> Parser a) -> ObjectParser a
liftObjectParser k p = OP (\obj -> explicitParseField p obj k) (HS.singleton k)
{-# INLINE liftObjectParser #-}

explicitObjectField :: Text -> (Value -> Parser a) -> ObjectParser a
explicitObjectField = liftObjectParser

objectField :: FromJSON a => Text -> ObjectParser a
objectField k = explicitObjectField k parseJSON

-- | Strict 'runObjectParser'.
--
-- First checks that there aren't extra keys in the 'Object'.
runObjectParser'
    :: ObjectParser a
    -> HS.HashSet Text  -- additional required keys, these keys MUST be present
    -> HS.HashSet Text  -- additional optional keys, these keys MAY be present
    -> Object
    -> Parser a
runObjectParser' (OP p ks) ks' os' obj
    | Just missing <- required `isSubsetOf` keys =
        -- take only few keys to have reasonable sized errors
        fail $ "Not all required keys present: " ++ show (take 3 $ HS.toList missing)
    | Just extra <-  keys `isSubsetOf` optional =
        fail $ "Extra keys present: " ++ show (take 3 $ HS.toList extra)
    | otherwise                        = p obj
  where
    keys = HS.fromMap (() <$ obj)
    required = HS.union ks ks'
    optional = HS.union required os'

-- Special case: Nothing = True, Just extraKeys = False
isSubsetOf :: HS.HashSet Text -> HS.HashSet Text -> Maybe (HS.HashSet Text)
isSubsetOf as bs
    | HS.null cs = Nothing
    | otherwise  = Just cs
  where
    cs = HS.difference as bs
