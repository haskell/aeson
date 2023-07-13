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
    decodeStrictText,
    eitherDecodeStrictText,
    throwDecodeStrictText,
    toEitherValue,
    unescapeText,
) where

import           Control.Monad.Catch                 (MonadThrow (..))
import           Data.Aeson.Types.Internal           (AesonException (..), formatError)

import qualified Data.Aeson.Types                    as A
import qualified Data.ByteString                     as BS
import qualified Data.ByteString.Lazy                as LBS
import qualified Data.Text                           as T

import           Data.Aeson.Decoding.ByteString
import           Data.Aeson.Decoding.ByteString.Lazy
import           Data.Aeson.Decoding.Text
import           Data.Aeson.Decoding.Conversion
import           Data.Aeson.Internal.Unescape        (unescapeText)

-------------------------------------------------------------------------------
-- Decoding: strict bytestring
-------------------------------------------------------------------------------

-- | Efficiently deserialize a JSON value from a strict 'B.ByteString'.
-- If this fails due to incomplete or invalid input, 'Nothing' is
-- returned.
decodeStrict :: (A.FromJSON a) => BS.ByteString -> Maybe a
decodeStrict bs = unResult (toResultValue (bsToTokens bs)) (\_ -> Nothing) $ \v bs' -> case A.ifromJSON v of
    A.ISuccess x
        | bsSpace bs' -> Just x
        | otherwise   -> Nothing
    A.IError _ _      -> Nothing

-- | Like 'decodeStrict' but returns an error message when decoding fails.
eitherDecodeStrict :: (A.FromJSON a) => BS.ByteString -> Either String a
eitherDecodeStrict bs = unResult (toResultValue (bsToTokens bs)) Left $ \v bs' -> case A.ifromJSON v of
    A.ISuccess x
        | bsSpace bs' -> Right x
        | otherwise   -> Left "Trailing garbage"
    A.IError path msg -> Left $ formatError path msg

-- | Like 'decodeStrict' but throws an 'AesonException' when decoding fails.
throwDecodeStrict :: forall a m. (A.FromJSON a, MonadThrow m) => BS.ByteString -> m a
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
decode :: (A.FromJSON a) => LBS.ByteString -> Maybe a
decode bs = unResult (toResultValue (lbsToTokens bs)) (\_ -> Nothing) $ \v bs' -> case A.ifromJSON v of
    A.ISuccess x
        | lbsSpace bs' -> Just x
        | otherwise    -> Nothing
    A.IError _ _       -> Nothing

-- | Like 'decode' but returns an error message when decoding fails.
eitherDecode :: (A.FromJSON a) => LBS.ByteString -> Either String a
eitherDecode bs = unResult (toResultValue (lbsToTokens bs)) Left $ \v bs' -> case A.ifromJSON v of
    A.ISuccess x
        | lbsSpace bs' -> Right x
        | otherwise    -> Left "Trailing garbage"
    A.IError path msg  -> Left $ formatError path msg

-- | Like 'decode' but throws an 'AesonException' when decoding fails.
--
-- 'throwDecode' is in @aeson@ since 2.1.2.0, but this variant is added later.
throwDecode :: forall a m. (A.FromJSON a, MonadThrow m) => LBS.ByteString -> m a
throwDecode bs = unResult (toResultValue (lbsToTokens bs)) (throwM . AesonException) $ \v bs' -> case A.ifromJSON v of
    A.ISuccess x
        | lbsSpace bs'  -> pure x
        | otherwise    -> throwM $ AesonException "Trailing garbage"
    A.IError path msg  -> throwM $ AesonException $ formatError path msg

-------------------------------------------------------------------------------
-- Decoding: strict text
-------------------------------------------------------------------------------

-- | Efficiently deserialize a JSON value from a strict 'B.ByteString'.
-- If this fails due to incomplete or invalid input, 'Nothing' is
-- returned.
--
-- @since 2.2.1.0
decodeStrictText :: (A.FromJSON a) => T.Text -> Maybe a
decodeStrictText bs = unResult (toResultValue (textToTokens bs)) (\_ -> Nothing) $ \v bs' -> case A.ifromJSON v of
    A.ISuccess x
        | textSpace bs' -> Just x
        | otherwise     -> Nothing
    A.IError _ _        -> Nothing

-- | Like 'decodeStrictText' but returns an error message when decoding fails.
--
-- @since 2.2.1.0
eitherDecodeStrictText :: (A.FromJSON a) => T.Text -> Either String a
eitherDecodeStrictText bs = unResult (toResultValue (textToTokens bs)) Left $ \v bs' -> case A.ifromJSON v of
    A.ISuccess x
        | textSpace bs' -> Right x
        | otherwise     -> Left "Trailing garbage"
    A.IError path msg   -> Left $ formatError path msg

-- | Like 'decodeStrictText' but throws an 'AesonException' when decoding fails.
--
-- @since 2.2.1.0
throwDecodeStrictText :: forall a m. (A.FromJSON a, MonadThrow m) => T.Text -> m a
throwDecodeStrictText bs = unResult (toResultValue (textToTokens bs)) (throwM . AesonException) $ \v bs' -> case A.ifromJSON v of
    A.ISuccess x
        | textSpace bs' -> pure x
        | otherwise     -> throwM $ AesonException "Trailing garbage"
    A.IError path msg   -> throwM $ AesonException $ formatError path msg
