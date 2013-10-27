{-# LANGUAGE CPP, DeriveDataTypeable, DeriveGeneric, TemplateHaskell #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Main where

--------------------------------------------------------------------------------

import Criterion.Main hiding (defaultOptions)
import Control.DeepSeq (NFData, rnf, deepseq)
import Data.Typeable (Typeable)
import Data.Data (Data)
import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy as Lazy (ByteString)
import Data.Aeson.Encode (encode)
import Data.Aeson.Types
import Data.Aeson.TH
import qualified Data.Aeson.Generic as G (fromJSON, toJSON, encode)

import Options

#ifdef FINALLY_TAGLESS
import Data.Aeson.Encode (jsonBuilderToLazyByteString)
#else
jsonBuilderToLazyByteString :: ToJSON a => a -> Lazy.ByteString
jsonBuilderToLazyByteString = encode
{-# INLINE jsonBuilderToLazyByteString #-}
#endif

--------------------------------------------------------------------------------

data D a = Nullary
         | Unary Int
         | Product String Char a
         | Record { testOne   :: Double
                  , testTwo   :: Bool
                  , testThree :: D a
                  }
           deriving (Show, Eq, Data, Typeable)

deriveJSON opts ''D

instance NFData a => NFData (D a) where
    rnf Nullary         = ()
    rnf (Unary n)       = rnf n
    rnf (Product s c x) = s `deepseq` c `deepseq` rnf x
    rnf (Record f b y)  = f `deepseq` b `deepseq` rnf y

type T = D (D (D ()))

d :: T
d = Record
    { testOne = 1234.56789
    , testTwo = True
    , testThree = Product "Hello World!" 'a' $
                    Record
                    { testOne   = 9876.54321
                    , testTwo   = False
                    , testThree = Product "Yeehaa!!!" '\n' Nullary
                    }
    }

--------------------------------------------------------------------------------

data D' a = Nullary'
          | Unary' Int
          | Product' String Char a
          | Record' { testOne'   :: Double
                    , testTwo'   :: Bool
                    , testThree' :: D' a
                    }
            deriving (Show, Eq, Generic, Data, Typeable)

instance ToJSON a => ToJSON (D' a) where
    toJSON = genericToJSON opts

instance FromJSON a => FromJSON (D' a) where
    parseJSON = genericParseJSON opts

instance NFData a => NFData (D' a) where
    rnf Nullary'         = ()
    rnf (Unary' n)       = rnf n
    rnf (Product' s c x) = s `deepseq` c `deepseq` rnf x
    rnf (Record' f b y)  = f `deepseq` b `deepseq` rnf y

type T' = D' (D' (D' ()))

d' :: T'
d' = Record'
    { testOne' = 1234.56789
    , testTwo' = True
    , testThree' = Product' "Hello World!" 'a' $
                    Record'
                    { testOne'   = 9876.54321
                    , testTwo'   = False
                    , testThree' = Product' "Yeehaa!!!" '\n' Nullary'
                    }
    }

--------------------------------------------------------------------------------

data BigRecord = BigRecord
    { field01 :: !Int, field02 :: !Int, field03 :: !Int, field04 :: !Int, field05 :: !Int
    , field06 :: !Int, field07 :: !Int, field08 :: !Int, field09 :: !Int, field10 :: !Int
    , field11 :: !Int, field12 :: !Int, field13 :: !Int, field14 :: !Int, field15 :: !Int
    , field16 :: !Int, field17 :: !Int, field18 :: !Int, field19 :: !Int, field20 :: !Int
    , field21 :: !Int, field22 :: !Int, field23 :: !Int, field24 :: !Int, field25 :: !Int
    } deriving (Show, Eq, Generic, Data, Typeable)

instance NFData BigRecord

bigRecord :: BigRecord
bigRecord = BigRecord 1   2  3  4  5
                      6   7  8  9 10
                      11 12 13 14 15
                      16 17 18 19 20
                      21 22 23 24 25

gBigRecordToJSON bigRecord = genericToJSON opts (bigRecord :: BigRecord)
{-# INLINE gBigRecordToJSON #-}

gBigRecordToValue :: BigRecord -> Value
gBigRecordToValue = gBigRecordToJSON
{-# INLINE gBigRecordToValue #-}

gEncodeBigRecord :: BigRecord -> Lazy.ByteString
gEncodeBigRecord = jsonBuilderToLazyByteString . gBigRecordToJSON
{-# INLINE gEncodeBigRecord #-}

gBigRecordFromJSON :: Value -> Result BigRecord
gBigRecordFromJSON = parse $ genericParseJSON opts
{-# INLINE gBigRecordFromJSON #-}

#ifdef FINALLY_TAGLESS
thBigRecordToJSON :: JSON json => BigRecord -> json
#endif
thBigRecordToJSON bigRecord = $(mkToJSON opts ''BigRecord) bigRecord
{-# INLINE thBigRecordToJSON #-}

thBigRecordToValue :: BigRecord -> Value
thBigRecordToValue = thBigRecordToJSON
{-# INLINE thBigRecordToValue #-}

thEncodeBigRecord :: BigRecord -> Lazy.ByteString
thEncodeBigRecord = jsonBuilderToLazyByteString . thBigRecordToJSON
{-# INLINE thEncodeBigRecord #-}

thBigRecordFromJSON :: Value -> Result BigRecord
thBigRecordFromJSON = parse $(mkParseJSON opts ''BigRecord)
{-# INLINE thBigRecordFromJSON #-}

--------------------------------------------------------------------------------

data BigProduct = BigProduct
    !Int !Int !Int !Int !Int
    !Int !Int !Int !Int !Int
    !Int !Int !Int !Int !Int
    !Int !Int !Int !Int !Int
    !Int !Int !Int !Int !Int
    deriving (Show, Eq, Generic, Data, Typeable)

instance NFData BigProduct

bigProduct :: BigProduct
bigProduct = BigProduct 1   2  3  4  5
                        6   7  8  9 10
                        11 12 13 14 15
                        16 17 18 19 20
                        21 22 23 24 25

gBigProductToJSON bigProduct = genericToJSON opts (bigProduct :: BigProduct)
{-# INLINE gBigProductToJSON #-}

gBigProductToValue :: BigProduct -> Value
gBigProductToValue = gBigProductToJSON
{-# INLINE gBigProductToValue #-}

gEncodeBigProduct :: BigProduct -> Lazy.ByteString
gEncodeBigProduct = jsonBuilderToLazyByteString . gBigProductToJSON
{-# INLINE gEncodeBigProduct #-}

gBigProductFromJSON :: Value -> Result BigProduct
gBigProductFromJSON = parse $ genericParseJSON opts
{-# INLINE gBigProductFromJSON #-}

#ifdef FINALLY_TAGLESS
thBigProductToJSON :: (JSON json) => BigProduct -> json
#endif
thBigProductToJSON bigProduct = $(mkToJSON opts ''BigProduct) bigProduct
{-# INLINE thBigProductToJSON #-}

thBigProductToValue :: BigProduct -> Value
thBigProductToValue = thBigProductToJSON
{-# INLINE thBigProductToValue #-}

thEncodeBigProduct :: BigProduct -> Lazy.ByteString
thEncodeBigProduct = jsonBuilderToLazyByteString . thBigProductToJSON
{-# INLINE thEncodeBigProduct #-}

thBigProductFromJSON :: Value -> Result BigProduct
thBigProductFromJSON = parse $(mkParseJSON opts ''BigProduct)
{-# INLINE thBigProductFromJSON #-}

--------------------------------------------------------------------------------

data BigSum = F01 | F02 | F03 | F04 | F05
            | F06 | F07 | F08 | F09 | F10
            | F11 | F12 | F13 | F14 | F15
            | F16 | F17 | F18 | F19 | F20
            | F21 | F22 | F23 | F24 | F25
    deriving (Show, Eq, Generic, Data, Typeable)

instance NFData BigSum

bigSum :: BigSum
bigSum = F25

gBigSumToJSON bigSum = genericToJSON opts (bigSum :: BigSum)
{-# INLINE gBigSumToJSON #-}

gBigSumToValue :: BigSum -> Value
gBigSumToValue = gBigSumToJSON
{-# INLINE gBigSumToValue #-}

gEncodeBigSum :: BigSum -> Lazy.ByteString
gEncodeBigSum = jsonBuilderToLazyByteString . gBigSumToJSON
{-# INLINE gEncodeBigSum #-}

gBigSumFromJSON :: Value -> Result BigSum
gBigSumFromJSON = parse $ genericParseJSON opts
{-# INLINE gBigSumFromJSON #-}

#ifdef FINALLY_TAGLESS
thBigSumToJSON :: (JSON json) => BigSum -> json
#endif
thBigSumToJSON bigSum = $(mkToJSON opts ''BigSum) bigSum
{-# INLINE thBigSumToJSON #-}

thBigSumToValue :: BigSum -> Value
thBigSumToValue = thBigSumToJSON
{-# INLINE thBigSumToValue #-}

thEncodeBigSum :: BigSum -> Lazy.ByteString
thEncodeBigSum = jsonBuilderToLazyByteString . thBigSumToJSON
{-# INLINE thEncodeBigSum #-}

thBigSumFromJSON :: Value -> Result BigSum
thBigSumFromJSON = parse $(mkParseJSON opts ''BigSum)
{-# INLINE thBigSumFromJSON #-}

--------------------------------------------------------------------------------

type FJ a = Value -> Result a

toValue :: ToJSON a => a -> Value
toValue = toJSON
{-# INLINE toValue #-}

toValueSyb :: Data a => a -> Value
toValueSyb = G.toJSON
{-# INLINE toValueSyb #-}

main :: IO ()
main = defaultMain
  [ bgroup "to/from Value"
    [ let v = toValue d
      in (d, d', v) `deepseq`
         bgroup "D"
         [ group "toJSON"   (nf toValue    d)
                            (nf toValueSyb d)
                            (nf toValue    d')
         , group "fromJSON" (nf (  fromJSON :: FJ T ) v)
                            (nf (G.fromJSON :: FJ T ) v)
                            (nf (  fromJSON :: FJ T') v)
         ]
    , let v = thBigRecordToJSON bigRecord :: Value
      in bigRecord `deepseq` v `deepseq`
         bgroup "BigRecord"
         [ group "toJSON"   (nf thBigRecordToValue bigRecord)
                            (nf toValueSyb         bigRecord)
                            (nf gBigRecordToValue  bigRecord)
         , group "fromJSON" (nf (thBigRecordFromJSON :: FJ BigRecord) v)
                            (nf (G.fromJSON          :: FJ BigRecord) v)
                            (nf (gBigRecordFromJSON  :: FJ BigRecord) v)
         ]
    , let v = thBigProductToJSON bigProduct
      in bigProduct `deepseq` v `deepseq`
         bgroup "BigProduct"
         [ group "toJSON"   (nf thBigProductToValue bigProduct)
                            (nf toValueSyb          bigProduct)
                            (nf gBigProductToValue  bigProduct)
         , group "fromJSON" (nf (thBigProductFromJSON :: FJ BigProduct) v)
                            (nf (G.fromJSON           :: FJ BigProduct) v)
                            (nf (gBigProductFromJSON  :: FJ BigProduct) v)
         ]
    , let v = thBigSumToJSON bigSum
      in bigSum `deepseq` v `deepseq`
         bgroup "BigSum"
         [ group "toJSON"   (nf thBigSumToValue bigSum)
                            (nf toValueSyb      bigSum)
                            (nf gBigSumToValue  bigSum)
         , group "fromJSON" (nf (thBigSumFromJSON :: FJ BigSum) v)
                            (nf (G.fromJSON       :: FJ BigSum) v)
                            (nf (gBigSumFromJSON  :: FJ BigSum) v)
         ]
    ]
  , bgroup "encode"
    [ (d, d') `deepseq`
        bgroup "D"
        [ group "toJSON"   (nf encode   d)
                           (nf G.encode d)
                           (nf encode   d')
        ]
    , bigRecord `deepseq`
        bgroup "BigRecord"
        [ group "toJSON"   (nf thEncodeBigRecord bigRecord)
                           (nf G.encode          bigRecord)
                           (nf gEncodeBigRecord  bigRecord)
        ]
    , bigProduct `deepseq`
        bgroup "BigProduct"
        [ group "toJSON"   (nf thEncodeBigProduct bigProduct)
                           (nf G.encode           bigProduct)
                           (nf gEncodeBigProduct  bigProduct)
        ]
    , bigSum `deepseq`
        bgroup "BigSum"
        [ group "toJSON"   (nf thEncodeBigSum bigSum)
                           (nf G.encode       bigSum)
                           (nf gEncodeBigSum  bigSum)
        ]
    ]
  ]

group :: (Benchmarkable b) => String -> b -> b -> b -> Benchmark
group n th syb gen = bcompare
                     [ bgroup n [ bench "th"      th
                                , bench "syb"     syb
                                , bench "generic" gen
                                ]
                     ]
