{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, TemplateHaskell #-}

module Main where

--------------------------------------------------------------------------------

import Criterion.Main hiding (defaultOptions)

import Control.DeepSeq (NFData, rnf, deepseq)

import Data.Typeable (Typeable)
import Data.Data (Data)
import GHC.Generics (Generic)

import Data.Aeson.Types
import Data.Aeson.TH (mkToJSON, mkParseJSON)
import qualified Data.Aeson.Generic as G (fromJSON, toJSON)

--------------------------------------------------------------------------------

-- Taken from the documentation of Data.Aeson.TH:
data D a = Nullary
         | Unary Int
         | Product String Char a
         | Record { testOne   :: Double
                  , testTwo   :: Bool
                  , testThree :: D a
                  } deriving (Show, Eq, Generic, Data, Typeable)

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
    , testThree = Product "Hello World!" 'a' $
                    Record
                    { testOne   = 9876.54321
                    , testTwo   = False
                    , testThree = Product "Yeehaa!!!" '\n' Nullary
                    }
    }

instance ToJSON   a => ToJSON   (D a)
instance FromJSON a => FromJSON (D a)

thDToJSON :: ToJSON a => D a -> Value
thDToJSON = $(mkToJSON defaultOptions ''D)

thDParseJSON :: FromJSON a => Value -> Parser (D a)
thDParseJSON = $(mkParseJSON defaultOptions ''D)

thDFromJSON :: FromJSON a => Value -> Result (D a)
thDFromJSON = parse thDParseJSON

--------------------------------------------------------------------------------

data BigRecord = BigRecord
    { field01 :: !Int, field02 :: !Int, field03 :: !Int, field04 :: !Int, field05 :: !Int
    , field06 :: !Int, field07 :: !Int, field08 :: !Int, field09 :: !Int, field10 :: !Int
    , field11 :: !Int, field12 :: !Int, field13 :: !Int, field14 :: !Int, field15 :: !Int
    , field16 :: !Int, field17 :: !Int, field18 :: !Int, field19 :: !Int, field20 :: !Int
    , field21 :: !Int, field22 :: !Int, field23 :: !Int, field24 :: !Int, field25 :: !Int
    } deriving (Show, Eq, Generic, Data, Typeable)

instance NFData BigRecord

bigRecord = BigRecord 1   2  3  4  5
                      6   7  8  9 10
                      11 12 13 14 15
                      16 17 18 19 20
                      21 22 23 24 25

instance ToJSON   BigRecord
instance FromJSON BigRecord

thBigRecordToJSON :: BigRecord -> Value
thBigRecordToJSON = $(mkToJSON defaultOptions ''BigRecord)

thBigRecordParseJSON :: Value -> Parser BigRecord
thBigRecordParseJSON = $(mkParseJSON defaultOptions ''BigRecord)

thBigRecordFromJSON :: Value -> Result BigRecord
thBigRecordFromJSON = parse thBigRecordParseJSON

--------------------------------------------------------------------------------

data BigProduct = BigProduct
    !Int !Int !Int !Int !Int
    !Int !Int !Int !Int !Int
    !Int !Int !Int !Int !Int
    !Int !Int !Int !Int !Int
    !Int !Int !Int !Int !Int
    deriving (Show, Eq, Generic, Data, Typeable)

instance NFData BigProduct

bigProduct = BigProduct 1   2  3  4  5
                        6   7  8  9 10
                        11 12 13 14 15
                        16 17 18 19 20
                        21 22 23 24 25

instance ToJSON   BigProduct
instance FromJSON BigProduct

thBigProductToJSON :: BigProduct -> Value
thBigProductToJSON = $(mkToJSON defaultOptions ''BigProduct)

thBigProductParseJSON :: Value -> Parser BigProduct
thBigProductParseJSON = $(mkParseJSON defaultOptions ''BigProduct)

thBigProductFromJSON :: Value -> Result BigProduct
thBigProductFromJSON = parse thBigProductParseJSON

--------------------------------------------------------------------------------

data BigSum = F01 | F02 | F03 | F04 | F05
            | F06 | F07 | F08 | F09 | F10
            | F11 | F12 | F13 | F14 | F15
            | F16 | F17 | F18 | F19 | F20
            | F21 | F22 | F23 | F24 | F25
    deriving (Show, Eq, Generic, Data, Typeable)

instance NFData BigSum

bigSum = F25

instance ToJSON   BigSum
instance FromJSON BigSum

thBigSumToJSON :: BigSum -> Value
thBigSumToJSON = $(mkToJSON defaultOptions ''BigSum)

thBigSumParseJSON :: Value -> Parser BigSum
thBigSumParseJSON = $(mkParseJSON defaultOptions ''BigSum)

thBigSumFromJSON :: Value -> Result BigSum
thBigSumFromJSON = parse thBigSumParseJSON

--------------------------------------------------------------------------------

type FJ a = Value -> Result a

main :: IO ()
main = defaultMain
  [ let v = thDToJSON d
    in d `deepseq` v `deepseq`
       bgroup "D"
       [ group "toJSON"   (nf thDToJSON d)
                          (nf G.toJSON  d)
                          (nf toJSON    d)
       , group "fromJSON" (nf (thDFromJSON :: FJ T) v)
                          (nf (G.fromJSON  :: FJ T) v)
                          (nf (fromJSON    :: FJ T) v)
       ]
  , let v = thBigRecordToJSON bigRecord
    in bigRecord `deepseq` v `deepseq`
       bgroup "BigRecord"
       [ group "toJSON"   (nf thBigRecordToJSON bigRecord)
                          (nf G.toJSON          bigRecord)
                          (nf toJSON            bigRecord)
       , group "fromJSON" (nf (thBigRecordFromJSON :: FJ BigRecord) v)
                          (nf (G.fromJSON          :: FJ BigRecord) v)
                          (nf (fromJSON            :: FJ BigRecord) v)
       ]
  , let v = thBigProductToJSON bigProduct
    in bigProduct `deepseq` v `deepseq`
       bgroup "BigProduct"
       [ group "toJSON"   (nf thBigProductToJSON bigProduct)
                          (nf G.toJSON           bigProduct)
                          (nf toJSON             bigProduct)
       , group "fromJSON" (nf (thBigProductFromJSON :: FJ BigProduct) v)
                          (nf (G.fromJSON           :: FJ BigProduct) v)
                          (nf (fromJSON             :: FJ BigProduct) v)
       ]
  , let v = thBigSumToJSON bigSum
    in bigSum `deepseq` v `deepseq`
       bgroup "BigSum"
       [ group "toJSON"   (nf thBigSumToJSON bigSum)
                          (nf G.toJSON       bigSum)
                          (nf toJSON         bigSum)
       , group "fromJSON" (nf (thBigSumFromJSON :: FJ BigSum) v)
                          (nf (G.fromJSON       :: FJ BigSum) v)
                          (nf (fromJSON         :: FJ BigSum) v)
       ]
  ]

group n th syb gen = bcompare
                     [ bgroup n [ bench "th"      th
                                , bench "syb"     syb
                                , bench "generic" gen
                                ]
                     ]
