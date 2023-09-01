{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Convertion to and from @aeson@ 'A.Value'.
-- 
module Data.Aeson.Decoding (
    decode,
    eitherDecode,
    throwDecode,
    decodeStrict,
    eitherDecodeStrict,
    throwDecodeStrict,
    toEitherValue,
    unescapeText,
) where

import           Control.Monad.Catch                 (MonadThrow (..))
import           Data.Aeson.Types.Internal           (AesonException (..), formatError)

import qualified Data.Aeson.Types                    as A
import qualified Data.ByteString                     as B
import qualified Data.ByteString.Lazy                as L

import           Data.Aeson.Decoding.ByteString
import           Data.Aeson.Decoding.ByteString.Lazy
import           Data.Aeson.Decoding.Conversion
import           Data.Aeson.Internal.Unescape        (unescapeText)

-------------------------------------------------------------------------------
-- Decoding: strict bytestring
-------------------------------------------------------------------------------

-- | Efficiently deserialize a JSON value from a strict 'B.ByteString'.
-- If this fails due to incomplete or invalid input, 'Nothing' is
-- returned.
decodeStrict :: (A.FromJSON a) => B.ByteString -> Maybe a
decodeStrict bs = unResult (toResultValue (bsToTokens bs)) (\_ -> Nothing) $ \v bs' -> case A.ifromJSON v of
    A.ISuccess x
        | bsSpace bs' -> Just x
        | otherwise   -> Nothing
    A.IError _ _      -> Nothing

-- | Like 'decodeStrict' but returns an error message when decoding fails.
eitherDecodeStrict :: (A.FromJSON a) => B.ByteString -> Either String a
eitherDecodeStrict bs = unResult (toResultValue (bsToTokens bs)) Left $ \v bs' -> case A.ifromJSON v of
    A.ISuccess x
        | bsSpace bs' -> Right x
        | otherwise   -> Left "Trailing garbage"
    A.IError path msg -> Left $ formatError path msg

-- | Like 'decodeStrict' but throws an 'AesonException' when decoding fails.
throwDecodeStrict :: forall a m. (A.FromJSON a, MonadThrow m) => B.ByteString -> m a
throwDecodeStrict bs = unResult (toResultValue (bsToTokens bs)) (throwM . AesonException) $ \v bs' -> case A.ifromJSON v of
    A.ISuccess x
        | bsSpace bs' -> pure x
        | otherwise   -> throwM $ AesonException "Trailing garbage"
    A.IError path msg -> throwM $ AesonException $ formatError path msg

-------------------------------------------------------------------------------
-- Decoding: lazy bytestring
-------------------------------------------------------------------------------

-- | Efficiently deserialize a JSON value from a lazy 'L.ByteString'.
-- If this fails due to incomplete or invalid input, 'Nothing' is
-- returned.
decode :: (A.FromJSON a) => L.ByteString -> Maybe a
decode bs = unResult (toResultValue (lbsToTokens bs)) (\_ -> Nothing) $ \v bs' -> case A.ifromJSON v of
    A.ISuccess x
        | lbsSpace bs' -> Just x
        | otherwise    -> Nothing
    A.IError _ _       -> Nothing

-- | Like 'decode' but returns an error message when decoding fails.
eitherDecode :: (A.FromJSON a) => L.ByteString -> Either String a
eitherDecode bs = unResult (toResultValue (lbsToTokens bs)) Left $ \v bs' -> case A.ifromJSON v of
    A.ISuccess x
        | lbsSpace bs' -> Right x
        | otherwise    -> Left "Trailing garbage"
    A.IError path msg  -> Left $ formatError path msg

-- | Like 'decode' but throws an 'AesonException' when decoding fails.
--
-- 'throwDecode' is in @aeson@ since 2.1.2.0, but this variant is added later.
throwDecode :: forall a m. (A.FromJSON a, MonadThrow m) => L.ByteString -> m a
throwDecode bs = unResult (toResultValue (lbsToTokens bs)) (throwM . AesonException) $ \v bs' -> case A.ifromJSON v of
    A.ISuccess x
        | lbsSpace bs'  -> pure x
        | otherwise    -> throwM $ AesonException "Trailing garbage"
    A.IError path msg  -> throwM $ AesonException $ formatError path msg
