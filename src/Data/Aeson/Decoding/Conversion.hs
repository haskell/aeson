{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes   #-}
module Data.Aeson.Decoding.Conversion (
    bsSpace,
    lbsSpace,
    textSpace,
    ltextSpace,
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
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT

import           Data.Aeson.Decoding.Tokens

bsSpace :: B.ByteString -> Bool
bsSpace = B.all (\w -> w == 0x20 || w == 0x0a || w == 0x0d || w == 0x09)

lbsSpace :: L.ByteString -> Bool
lbsSpace = L.all (\w -> w == 0x20 || w == 0x0a || w == 0x0d || w == 0x09)

textSpace :: T.Text -> Bool
textSpace = T.all (\c -> c == ' ' || c == '\r' || c == '\n' || c == '\t')

ltextSpace :: LT.Text -> Bool
ltextSpace = LT.all (\c -> c == ' ' || c == '\r' || c == '\n' || c == '\t')

-- | Convert 'Tokens' to 'A.Value'.
--
-- The resulting value will be in normal form if its forced.
-- In other words, there shouldn't be thunks inside.
--
toEitherValue
    :: Tokens k e             -- ^ tokens
    -> Either e (A.Value, k)  -- ^ either token error or value and leftover.
toEitherValue t = unResult (toResultValue t) Left $ \v k -> Right (v, k)

toResultValue
    :: Tokens k e           -- ^ tokens
    -> Result e k A.Value   -- ^ either token error or value and leftover.
toResultValue t0 = Result (convert t0)

convert :: Tokens k e -> (e -> r) -> (A.Value -> k -> r) -> r
convert (TkLit l k)        _ f = f (lit l) k where
    lit :: Lit -> A.Value
    lit LitNull  = A.Null
    lit LitTrue  = A.Bool True
    lit LitFalse = A.Bool False
convert (TkText t k)       _ f = f (A.String t) k
convert (TkNumber n k)     _ f = f (A.Number (num n)) k where
    num :: Number -> Scientific
    num (NumInteger m)    = fromInteger m
    num (NumDecimal s)    = s
    num (NumScientific s) = s
convert (TkArrayOpen arr)  g f = convertA arr g $ \xs k -> f (A.Array xs) k
convert (TkRecordOpen rec) g f = convertR rec g $ \xs k -> f (A.Object xs) k
convert (TkErr e)          g _ = g e

convertA :: TkArray k e -> (e -> r) -> (A.Array -> k -> r) -> r
convertA tka err kont = goA 0 id tka err $ \n xs -> kont (V.fromListN n xs) where
    goA :: Int                           -- size accumulator
        -> ([A.Value] -> [A.Value])      -- dlist accumulator
        -> TkArray k e                   -- array tokens
        -> (e -> r)                      -- error continuation
        -> (Int -> [A.Value] -> k -> r)  -- success continuation
        -> r
    goA !n !acc (TkItem toks)  g f = convert toks g $ \ !v k -> goA (succ n) (acc . (v :)) k g f
    goA !n !acc (TkArrayEnd k) _ f = f n (acc []) k
    goA !_ !_   (TkArrayErr e) g _ = g e

convertR :: TkRecord k e -> (e -> r) -> (A.Object -> k -> r) -> r
convertR tkr err kont = goR [] tkr err $ kont . KM.fromList where
    -- we accumulate keys in reverse order
    -- then the first duplicate key in objects wins (as KM.fromList picks last).
    goR :: [(Key, A.Value)]
        -> TkRecord k e
        -> (e -> r)
        -> ([(Key, A.Value)] -> k -> r)
        -> r
    -- here we don't stricly need bang on !v as KM is a Strict (in values) map.
    -- but we force the value sooner.
    goR !acc (TkPair t toks) g f = convert toks g $ \ !v k -> goR ((t , v) : acc) k g f
    goR !acc (TkRecordEnd k) _ f = f acc k
    goR !_   (TkRecordErr e) g _ = g e

newtype Result e k a = Result
    { unResult :: forall r. (e -> r) -> (a -> k -> r) -> r }
