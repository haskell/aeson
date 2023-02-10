{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Convertion to and from @aeson@ 'A.Value'.
module Data.Aeson.Decoding (
    decode,
    eitherDecode,
    throwDecode,
    decodeStrict,
    eitherDecodeStrict,
    throwDecodeStrict,
    toEitherValue,
) where

import           Control.Monad.Catch                 (MonadThrow (..))
import           Data.Aeson.Key                      (Key)
import           Data.Aeson.Types.Internal           (formatError)
import           Data.Scientific                     (Scientific)

import qualified Data.Aeson.KeyMap                   as KM
import qualified Data.Aeson.Types                    as A
import qualified Data.ByteString                     as B
import qualified Data.ByteString.Lazy                as L
import qualified Data.Vector                         as V

import           Data.Aeson                          (AesonException (..))
import           Data.Aeson.Decoding.ByteString
import           Data.Aeson.Decoding.ByteString.Lazy
import           Data.Aeson.Decoding.Tokens

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

-- | Efficiently deserialize a JSON value from a strict 'B.ByteString'.
-- If this fails due to incomplete or invalid input, 'Nothing' is
-- returned.
decode :: (A.FromJSON a) => L.ByteString -> Maybe a
decode bs = unResult (toResultValue (lbsToTokens bs)) (\_ -> Nothing) $ \v bs' -> case A.ifromJSON v of
    A.ISuccess x
        | lbsSpace bs' -> Just x
        | otherwise    -> Nothing
    A.IError _ _       -> Nothing

-- | Like 'decodeStrict' but returns an error message when decoding fails.
eitherDecode :: (A.FromJSON a) => L.ByteString -> Either String a
eitherDecode bs = unResult (toResultValue (lbsToTokens bs)) Left $ \v bs' -> case A.ifromJSON v of
    A.ISuccess x
        | lbsSpace bs' -> Right x
        | otherwise    -> Left "Trailing garbage"
    A.IError path msg  -> Left $ formatError path msg

-- | Like 'decode' but throws an 'AesonException' when decoding fails.
throwDecode :: forall a m. (A.FromJSON a, MonadThrow m) => L.ByteString -> m a
throwDecode bs = unResult (toResultValue (lbsToTokens bs)) (throwM . AesonException) $ \v bs' -> case A.ifromJSON v of
    A.ISuccess x
        | lbsSpace bs'  -> pure x
        | otherwise    -> throwM $ AesonException "Trailing garbage"
    A.IError path msg  -> throwM $ AesonException $ formatError path msg

-------------------------------------------------------------------------------
-- Conversions
-------------------------------------------------------------------------------

bsSpace :: B.ByteString -> Bool
bsSpace = B.all (\w -> w == 0x20 || w == 0x0a || w == 0x0d || w == 0x09)

lbsSpace :: L.ByteString -> Bool
lbsSpace = L.all (\w -> w == 0x20 || w == 0x0a || w == 0x0d || w == 0x09)

-- | Convert 'Tokens' to 'A.Value'.
toEitherValue
    :: Tokens k e             -- ^ tokens
    -> Either e (A.Value, k)  -- ^ either token error or value and leftover.
toEitherValue t = unResult (toResultValue t) Left $ \v k -> Right (v, k)

toResultValue
    :: Tokens k e           -- ^ tokens
    -> Result e k A.Value  -- ^ either token error or value and leftover.
toResultValue t0 = Result (go t0) where
    go :: Tokens k e -> (e -> r) -> (A.Value -> k -> r) -> r
    go (TkLit l k)        _ f = f (lit l) k
    go (TkText t k)       _ f = f (A.String t) k
    go (TkNumber n k)     _ f = f (A.Number (num n)) k
    go (TkArrayOpen arr)  g f = goA 0 id arr g $ \n xs k -> f (A.Array (V.fromListN n xs)) k
    go (TkRecordOpen rec) g f = goR [] rec g $ \xs k -> f (A.Object (KM.fromList xs)) k
    go (TkErr e)          g _ = g e

    lit :: Lit -> A.Value
    lit LitNull  = A.Null
    lit LitTrue  = A.Bool True
    lit LitFalse = A.Bool False

    num :: Number -> Scientific
    num (NumInteger n)    = fromInteger n
    num (NumDecimal s)    = s
    num (NumScientific s) = s

    goA :: Int                           -- size accumulator
        -> ([A.Value] -> [A.Value])      -- dlist accumulator
        -> TkArray k e                   -- array tokens
        -> (e -> r)                      -- error continuation
        -> (Int -> [A.Value] -> k -> r)  -- success continuation
        -> r
    goA !n !acc (TkItem toks)  g f = go toks g $ \v k -> goA (succ n) (acc . (v :)) k g f
    goA !n !acc (TkArrayEnd k) _ f = f n (acc []) k
    goA !_ !_   (TkArrayErr e) g _ = g e

    -- we accumulate keys in reverse order
    -- then the first duplicate key in objects wins (as KM.fromList picks last).
    goR :: [(Key, A.Value)]
        -> TkRecord k e
        -> (e -> r)
        -> ([(Key, A.Value)] -> k -> r)
        -> r
    goR !acc (TkPair t toks) g f = go toks g $ \v k -> goR ((t , v) : acc) k g f
    goR !acc (TkRecordEnd k) _ f = f acc k
    goR !_   (TkRecordErr e) g _ = g e

newtype Result e k a = Result
    { unResult :: forall r. (e -> r) -> (a -> k -> r) -> r }
