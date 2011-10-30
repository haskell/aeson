{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, TemplateHaskell #-}

module Main where

--------------------------------------------------------------------------------

import Criterion.Main

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
                  } deriving (Eq, Generic, Data, Typeable)

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
thDToJSON = $(mkToJSON id ''D)

thDParseJSON :: FromJSON a => Value -> Parser (D a)
thDParseJSON = $(mkParseJSON id ''D)

thDFromJSON :: FromJSON a => Value -> Result (D a)
thDFromJSON = parse thDParseJSON

--------------------------------------------------------------------------------

data BigRecord = BigRecord
    { field01 :: !(), field02 :: !(), field03 :: !(), field04 :: !(), field05 :: !()
    , field06 :: !(), field07 :: !(), field08 :: !(), field09 :: !(), field10 :: !()
    , field11 :: !(), field12 :: !(), field13 :: !(), field14 :: !(), field15 :: !()
    , field16 :: !(), field17 :: !(), field18 :: !(), field19 :: !(), field20 :: !()
    , field21 :: !(), field22 :: !(), field23 :: !(), field24 :: !(), field25 :: !()
    } deriving (Eq, Generic, Data, Typeable)

instance NFData BigRecord

bigRecord = BigRecord () () () () ()
                      () () () () ()
                      () () () () ()
                      () () () () ()
                      () () () () ()

instance ToJSON   BigRecord
instance FromJSON BigRecord

thBigRecordToJSON :: BigRecord -> Value
thBigRecordToJSON = $(mkToJSON id ''BigRecord)

thBigRecordParseJSON :: Value -> Parser BigRecord
thBigRecordParseJSON = $(mkParseJSON id ''BigRecord)

thBigRecordFromJSON :: Value -> Result BigRecord
thBigRecordFromJSON = parse thBigRecordParseJSON

--------------------------------------------------------------------------------

data BigProduct = BigProduct
    !() !() !() !() !()
    !() !() !() !() !()
    !() !() !() !() !()
    !() !() !() !() !()
    !() !() !() !() !()
    deriving (Eq, Generic, Data, Typeable)

instance NFData BigProduct

bigProduct = BigProduct () () () () ()
                        () () () () ()
                        () () () () ()
                        () () () () ()
                        () () () () ()

instance ToJSON   BigProduct
instance FromJSON BigProduct

thBigProductToJSON :: BigProduct -> Value
thBigProductToJSON = $(mkToJSON id ''BigProduct)

thBigProductParseJSON :: Value -> Parser BigProduct
thBigProductParseJSON = $(mkParseJSON id ''BigProduct)

thBigProductFromJSON :: Value -> Result BigProduct
thBigProductFromJSON = parse thBigProductParseJSON

--------------------------------------------------------------------------------

data BigSum = F01 | F02 | F03 | F04 | F05
            | F06 | F07 | F08 | F09 | F10
            | F11 | F12 | F13 | F14 | F15
            | F16 | F17 | F18 | F19 | F20
            | F21 | F22 | F23 | F24 | F25
    deriving (Eq, Generic, Data, Typeable)

instance NFData BigSum

bigSum = F12

instance ToJSON   BigSum
instance FromJSON BigSum

thBigSumToJSON :: BigSum -> Value
thBigSumToJSON = $(mkToJSON id ''BigSum)

thBigSumParseJSON :: Value -> Parser BigSum
thBigSumParseJSON = $(mkParseJSON id ''BigSum)

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

group n th syb gen = bgroup n [ bench "th"      th
                              , bench "syb"     syb
                              , bench "generic" gen
                              ]
