{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Prelude ()
import Prelude.Compat

import Control.Monad
import Control.DeepSeq (NFData, rnf, deepseq)
import Criterion.Main hiding (defaultOptions)
import Data.Aeson
import Data.Aeson.Encoding
import Data.Aeson.TH
import Data.Aeson.Types
import Data.ByteString.Lazy (ByteString)
import Data.Data (Data)
import Data.Typeable (Typeable)
import GHC.Generics (Generic, Rep)
import Options

toBS :: Encoding -> ByteString
toBS = encodingToLazyByteString

gEncode :: (Generic a, GToEncoding Zero (Rep a)) => a -> ByteString
gEncode = toBS . genericToEncoding opts

--------------------------------------------------------------------------------

data D a = Nullary
         | Unary Int
         | Product String Char a
         | Record { testOne   :: Double
                  , testTwo   :: Bool
                  , testThree :: D a
                  }
           deriving (Show, Eq)

deriveJSON opts ''D

instance NFData a => NFData (D a) where
    rnf Nullary         = ()
    rnf (Unary n)       = rnf n
    rnf (Product s c x) = s `deepseq` c `deepseq` rnf x
    rnf (Record d b y)  = d `deepseq` b `deepseq` rnf y

type T = D (D (D ()))

d :: T
d = Record
    { testOne = 1234.56789
    , testTwo = True
    , testThree = Product "Hello World!" 'a'
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
            deriving (Show, Eq, Generic)

instance ToJSON a => ToJSON (D' a) where
    toJSON = genericToJSON opts

instance FromJSON a => FromJSON (D' a) where
    parseJSON = genericParseJSON opts

instance NFData a => NFData (D' a) where
    rnf Nullary'         = ()
    rnf (Unary' n)       = rnf n
    rnf (Product' s c x) = s `deepseq` c `deepseq` rnf x
    rnf (Record' d b y)  = d `deepseq` b `deepseq` rnf y

type T' = D' (D' (D' ()))

d' :: T'
d' = Record'
    { testOne' = 1234.56789
    , testTwo' = True
    , testThree' = Product' "Hello World!" 'a'
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
    } deriving (Show, Eq, Generic)

instance NFData BigRecord

bigRecord = BigRecord 1   2  3  4  5
                      6   7  8  9 10
                      11 12 13 14 15
                      16 17 18 19 20
                      21 22 23 24 25

return []

gBigRecordToJSON :: BigRecord -> Value
gBigRecordToJSON = genericToJSON opts

gBigRecordEncode :: BigRecord -> ByteString
gBigRecordEncode = gEncode

gBigRecordFromJSON :: Value -> Result BigRecord
gBigRecordFromJSON = parse $ genericParseJSON opts

thBigRecordToJSON :: BigRecord -> Value
thBigRecordToJSON = $(mkToJSON opts ''BigRecord)

thBigRecordEncode :: BigRecord -> ByteString
thBigRecordEncode = toBS . $(mkToEncoding opts ''BigRecord)

thBigRecordFromJSON :: Value -> Result BigRecord
thBigRecordFromJSON = parse $(mkParseJSON opts ''BigRecord)

--------------------------------------------------------------------------------

data BigProduct = BigProduct
    !Int !Int !Int !Int !Int
    !Int !Int !Int !Int !Int
    !Int !Int !Int !Int !Int
    !Int !Int !Int !Int !Int
    !Int !Int !Int !Int !Int
    deriving (Show, Eq, Generic)

instance NFData BigProduct

bigProduct = BigProduct 1   2  3  4  5
                        6   7  8  9 10
                        11 12 13 14 15
                        16 17 18 19 20
                        21 22 23 24 25

return []

gBigProductToJSON :: BigProduct -> Value
gBigProductToJSON = genericToJSON opts

gBigProductEncode :: BigProduct -> ByteString
gBigProductEncode = gEncode

gBigProductFromJSON :: Value -> Result BigProduct
gBigProductFromJSON = parse $ genericParseJSON opts

thBigProductToJSON :: BigProduct -> Value
thBigProductToJSON = $(mkToJSON opts ''BigProduct)

thBigProductEncode :: BigProduct -> ByteString
thBigProductEncode = toBS . $(mkToEncoding opts ''BigProduct)

thBigProductFromJSON :: Value -> Result BigProduct
thBigProductFromJSON = parse $(mkParseJSON opts ''BigProduct)

--------------------------------------------------------------------------------

data BigSum = F01 | F02 | F03 | F04 | F05
            | F06 | F07 | F08 | F09 | F10
            | F11 | F12 | F13 | F14 | F15
            | F16 | F17 | F18 | F19 | F20
            | F21 | F22 | F23 | F24 | F25
    deriving (Show, Eq, Generic)

instance NFData BigSum

bigSum = F25

return []

gBigSumToJSON :: BigSum -> Value
gBigSumToJSON = genericToJSON opts

gBigSumEncode :: BigSum -> ByteString
gBigSumEncode = gEncode

gBigSumFromJSON :: Value -> Result BigSum
gBigSumFromJSON = parse $ genericParseJSON opts

thBigSumToJSON :: BigSum -> Value
thBigSumToJSON = $(mkToJSON opts ''BigSum)

thBigSumEncode :: BigSum -> ByteString
thBigSumEncode = toBS . $(mkToEncoding opts ''BigSum)

thBigSumFromJSON :: Value -> Result BigSum
thBigSumFromJSON = parse $(mkParseJSON opts ''BigSum)

--------------------------------------------------------------------------------

type FJ a = Value -> Result a

runBench :: IO ()
runBench = defaultMain
  [ let v = toJSON d
    in (d, d', v) `deepseq`
       bgroup "D"
       [ group "toJSON"   (nf   toJSON d)
                          (nf   toJSON d')
       , group "encode"   (nf encode d)
                          (nf encode d')
       , group "fromJSON" (nf (  fromJSON :: FJ T ) v)
                          (nf (  fromJSON :: FJ T') v)
       ]
  , let v = thBigRecordToJSON bigRecord
    in bigRecord `deepseq` v `deepseq`
       bgroup "BigRecord"
       [ group "toJSON"   (nf thBigRecordToJSON bigRecord)
                          (nf  gBigRecordToJSON bigRecord)
       , group "encode"   (nf thBigRecordEncode bigRecord)
                          (nf  gBigRecordEncode bigRecord)
       , group "fromJSON" (nf (thBigRecordFromJSON :: FJ BigRecord) v)
                          (nf ( gBigRecordFromJSON :: FJ BigRecord) v)
       ]
  , let v = thBigProductToJSON bigProduct
    in bigProduct `deepseq` v `deepseq`
       bgroup "BigProduct"
       [ group "toJSON"   (nf thBigProductToJSON bigProduct)
                          (nf gBigProductToJSON  bigProduct)
       , group "encode"   (nf thBigProductEncode bigProduct)
                          (nf  gBigProductEncode bigProduct)
       , group "fromJSON" (nf (thBigProductFromJSON :: FJ BigProduct) v)
                          (nf (gBigProductFromJSON  :: FJ BigProduct) v)
       ]
  , let v = thBigSumToJSON bigSum
    in bigSum `deepseq` v `deepseq`
       bgroup "BigSum"
       [ group "toJSON"   (nf thBigSumToJSON bigSum)
                          (nf gBigSumToJSON  bigSum)
       , group "encode"   (nf thBigSumEncode bigSum)
                          (nf  gBigSumEncode bigSum)
       , group "fromJSON" (nf (thBigSumFromJSON :: FJ BigSum) v)
                          (nf (gBigSumFromJSON  :: FJ BigSum) v)
       ]
  ]

group n th gen = bgroup n [ bench "th"      th
                          , bench "generic" gen
                          ]

sanityCheck = do
  check d toJSON fromJSON encode
  check d' toJSON fromJSON encode
  check bigRecord thBigRecordToJSON thBigRecordFromJSON thBigRecordEncode
  check bigRecord gBigRecordToJSON gBigRecordFromJSON gBigRecordEncode
  check bigProduct thBigProductToJSON thBigProductFromJSON thBigProductEncode
  check bigProduct gBigProductToJSON gBigProductFromJSON gBigProductEncode
  check bigSum thBigSumToJSON thBigSumFromJSON thBigSumEncode
  check bigSum gBigSumToJSON gBigSumFromJSON gBigSumEncode

check :: (Show a, Eq a)
      => a -> (a -> Value) -> (Value -> Result a) -> (a -> ByteString) -> IO ()
check x toJSON fromJSON encode = do
  unless (Success x == (fromJSON . toJSON) x) $ fail $ "toJSON: " ++ show x
  unless (Success x == (decode' . encode) x) $ fail $ "encode: " ++ show x
  where
    decode' s = case decode s of
      Just v -> fromJSON v
      Nothing -> fail ""

main = do
  sanityCheck
  runBench
