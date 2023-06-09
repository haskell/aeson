{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes   #-}
module Data.Aeson.Decoding.Conversion (
    bsSpace,
    lbsSpace,
    toEitherValue,
    toResultValue,
    Result (..),
) where

import           Data.Aeson.Key             (Key)
import           Data.Scientific            (Scientific)

import qualified Data.Aeson.KeyMap          as KM
import qualified Data.Aeson.Types.Internal  as A
import qualified Data.ByteString            as B
import qualified Data.ByteString.Lazy       as L
import qualified Data.Vector                as V

import           Data.Aeson.Decoding.Tokens

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
    -> Result e k A.Value   -- ^ either token error or value and leftover.
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
